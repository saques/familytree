{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Utils where

import Api.Types (User(User))
import Snap.Core
import Snap.Snaplet
import Control.Applicative
import Web.JWT
import Crypto.BCrypt


import Data.Text
import Data.Map
import Data.Maybe
import Data.Aeson
import Data.ByteString.Lazy
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as B



mapSecond :: (b -> c) -> (a,b) -> (a,c)
mapSecond f (a,b) = (a,f b)

addCORS :: Handler b d ()
addCORS = do
    modifyResponse $ setHeader "Access-Control-Allow-Origin" "*"

authenticate :: Handler b d ()
authenticate = do 
    rq <- getRequest
    let mh = getHeader "Token" rq
    if validateMaybeJWT mh
        then do
            modifyResponse $ setResponseCode 200
        else do 
            modifyResponse $ setResponseCode 401
            r <- getResponse
            finishWith r


checkPassword :: User -> B.ByteString -> Bool
checkPassword (User id name password) givenPassword = validatePassword (encodeUtf8 password) givenPassword
    
hash :: B.ByteString -> Maybe B.ByteString
hash password = 
    case maybeSalt of
        Nothing -> Nothing
        Just salt -> hashPassword password salt
    where maybeSalt = genSalt defaultHashAlgorithm 4 "kEwRYWa363DUKTMMAkPaCtb0hIbtlcAPVzZgbYNDT5Q"


createClaimsSet :: B.ByteString -> JWTClaimsSet
createClaimsSet user = JWTClaimsSet {
                    iss = Nothing, 
                    sub = Nothing, 
                    aud = Nothing, 
                    Web.JWT.exp = Nothing, 
                    nbf = Nothing, 
                    iat = Nothing, 
                    jti = Nothing, 
                    unregisteredClaims = ClaimsMap {
                        unClaimsMap = fromList [("username", String (decodeUtf8 user))]
                    }
                } 

joseHeader :: JOSEHeader
joseHeader = JOSEHeader {typ = Just "JWT", cty = Nothing, alg = Just HS256, kid = Nothing}


jwtSigned :: B.ByteString -> Text
jwtSigned user = encodeSigned getSigner joseHeader (createClaimsSet user)


getSigner :: Signer
getSigner = HMACSecret "6916971E368E3D3BEEBA7BCCF44D3"


validateJWT :: B.ByteString -> Bool
validateJWT rawJWT =
    case maybeValidJWT of
        Nothing -> False
        Just validJwt -> True
    where maybeValidJWT = decodeAndVerifySignature getSigner (decodeUtf8 rawJWT)


validateMaybeJWT :: Maybe B.ByteString -> Bool
validateMaybeJWT maybeRawJWT = 
    case maybeRawJWT of
        Nothing -> False
        Just rawJWT -> validateJWT rawJWT





