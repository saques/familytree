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

import Crypto.BCrypt

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


checkPassword :: User -> B.ByteString -> Bool
checkPassword (User id name password) givenPassword = validatePassword (encodeUtf8 password) givenPassword

hash :: B.ByteString -> Maybe B.ByteString
hash password = 
    case maybeSalt of
        Nothing -> Nothing
        Just salt -> hashPassword password salt
    where maybeSalt = genSalt defaultHashAlgorithm 4 "kEwRYWa363DUKTMMAkPaCtb0hIbtlcAPVzZgbYNDT5Q"



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

    if Prelude.null $ (users :: [User])
        then do
            let maybeHashed = hash password
            case maybeHashed of 
                Just hashed -> do
                    execute "INSERT INTO users (username, password) VALUES (?, ?)" [username, hashed]
                    modifyResponse $ setResponseCode 201
                    writeLBS "User created"
                Nothing -> do
                    modifyResponse $ setResponseCode 500
                    writeBS "Error hashing password"
        else do 
            modifyResponse $ setResponseCode 403
            writeLBS "User already exists"


userControllerInit :: Snaplet Postgres -> SnapletInit b UserController
userControllerInit db = makeSnaplet "users" "User Controller" Nothing $ do
    addRoutes apiRoutes
    return $ UserController db


instance HasPostgres (Handler b UserController) where
    getPostgresState = with db get