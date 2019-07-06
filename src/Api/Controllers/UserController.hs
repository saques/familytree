{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Controllers.UserController (
    userControllerInit, 
    UserController
    ) where

import Api.Types (User(User))
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

mapSecond :: (b -> c) -> (a,b) -> (a,c)
mapSecond f (a,b) = (a,f b)

addCORS :: Handler b UserController ()
addCORS = do
    modifyResponse $ setHeader "Access-Control-Allow-Origin" "*"

apiRoutes :: [(B.ByteString, Handler b UserController ())]
apiRoutes = Prelude.map (mapSecond (addCORS >>))
            [("/", method GET getUsers),
             ("/:username", method GET getUserByName),
             ("/:username", method POST postUser),
             ("/login/:username", method POST login)]


validatePassword :: User -> B.ByteString -> Bool
validatePassword (User id name password) givenPassword
        | encodeUtf8 password == givenPassword = True
        | otherwise                 = False


login :: Handler b UserController ()
login = do
    maybeUsername <- getParam "username"
    maybePassword <- getPostParam "password"
    let username = fromMaybe "" maybeUsername
        password = fromMaybe "" maybePassword

    users <- query "SELECT * FROM users WHERE username = ?" (Only username)
    
    if Prelude.null users
        then modifyResponse $ setResponseCode 404
        else if validatePassword (Prelude.head users) password 
            then modifyResponse $ setResponseCode 200
            else modifyResponse $ setResponseCode 401

    modifyResponse $ setHeader "Content-Type" "application/json"
    writeLBS "{ \"imagine\": \"this is a JWT\"}"

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
    let anyMatches = Prelude.null users

    if Prelude.null users
        then do 
            execute "INSERT INTO users (username, password) VALUES (?, ?)" [username, password]
            modifyResponse $ setResponseCode 201
        else do 
            modifyResponse $ setResponseCode 400

    --I simply dont understand why I need to write this for it to work
    modifyResponse $ setHeader "Content-Type" "application/json"
    writeLBS . encode $ (users :: [User])


userControllerInit :: Snaplet Postgres -> SnapletInit b UserController
userControllerInit db = makeSnaplet "users" "User Controller" Nothing $ do
    addRoutes apiRoutes
    return $ UserController db


instance HasPostgres (Handler b UserController) where
    getPostgresState = with db get