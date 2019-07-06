{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Controllers.UserController (
    userControllerInit, 
    UserController
    ) where

import Api.Types (User(User))
import Api.Utils
import Control.Lens (makeLenses)
import Snap.Core
import Snap.Snaplet
import Control.Applicative
import Snap.Snaplet.PostgresqlSimple
import Control.Monad.State.Class (get)
import Data.Aeson (encode)

import Data.Maybe
import Data.ByteString.Lazy
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as B

data UserController = UserController 
    {
        _db   :: Snaplet Postgres
    }

makeLenses ''UserController

apiRoutes :: [(B.ByteString, Handler b UserController ())]
apiRoutes = Prelude.map (mapSecond (addCORS >>))
            [("/", method GET getUsers),
             ("/:username", method GET getUserByName),
             ("/:username", method POST postUser),
             ("/login/:username", method POST login)]


authRoutes :: [(B.ByteString, Handler b UserController ())]
authRoutes =  Prelude.map (mapSecond (addCORS >> authenticate >>))
              [("/authenticated", method GET sampleAuthenticatedMethod)]

login :: Handler b UserController ()
login = do
    maybeUsername <- getParam "username"
    maybePassword <- getPostParam "password"
    let username = fromMaybe "" maybeUsername
        password = fromMaybe "" maybePassword

    users <- query "SELECT * FROM users WHERE username = ?" (Only username)
    
    if Prelude.null users
        then modifyResponse $ setResponseCode 404
        else if checkPassword (Prelude.head users) password 
            then do 
                modifyResponse $ setHeader "Token" (encodeUtf8 (jwtSigned username))
                modifyResponse $ setResponseCode 200
            else do 
                modifyResponse $ setResponseCode 401

getUserByName :: Handler b UserController ()
getUserByName = do
    maybeUsername <- getParam "username"
    let username = fromMaybe "" maybeUsername
    users <- query "SELECT * FROM users WHERE username = ?" (Only username)
    modifyResponse $ setHeader "Content-Type" "application/json"
    writeLBS . encode $ (users :: [User])

getUsers :: Handler b UserController ()
getUsers = do 
    users <- query_ "SELECT * FROM users"
    modifyResponse $ setHeader "Content-Type" "application/json"
    writeLBS . encode $ (users :: [User])


postUser :: Handler b UserController ()
postUser = do 
    --readRequestBody <size>
    maybeUsername <- getParam "username"
    maybePassword <- getPostParam "password"

    let username = fromMaybe "" maybeUsername
        password = fromMaybe "" maybePassword

    users <- query "SELECT * FROM users WHERE username = ?" (Only username)

    if Prelude.null $ (users :: [User])
        then do
            let maybeHashed = hash password
            case maybeHashed of 
                Just hashed -> do
                    execute "INSERT INTO users (username, password) VALUES (?, ?)" [username, hashed]
                    modifyResponse $ setResponseCode 201
                    modifyResponse $ setHeader "Token" (encodeUtf8 (jwtSigned username))
                    writeLBS "User created"
                Nothing -> do
                    modifyResponse $ setResponseCode 500
                    writeBS "Error hashing password"
        else do 
            modifyResponse $ setResponseCode 403
            writeLBS "User already exists"

--------------------------------------------------------

sampleAuthenticatedMethod :: Handler b UserController ()
sampleAuthenticatedMethod = do
    writeLBS "You are logged in!"

--------------------------------------------------------

userControllerInit :: Snaplet Postgres -> SnapletInit b UserController
userControllerInit db = makeSnaplet "users" "User Controller" Nothing $ do
    addRoutes apiRoutes
    addRoutes authRoutes
    return $ UserController db


instance HasPostgres (Handler b UserController) where
    getPostgresState = with db get