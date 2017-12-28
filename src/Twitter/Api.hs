{-# LANGUAGE OverloadedStrings #-}
module Twitter.Api where

import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Aeson hiding (json)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM

import Network.HTTP.Types.Status
import Web.Scotty

import Web.Authenticate.OAuth

import Twitter.Auth


type CredentialCache = TVar (Map Token Credential)

signinHandler :: CredentialCache -> ActionM ()
signinHandler cache = do
    originHostMb <- header "Host"
    let originCallback = (\h -> "http://" ++ (LT.unpack h) ++ "/callback") <$> originHostMb
    (creds, url) <- liftIO $ obtainRequestToken originCallback
    case oauthToken creds of
        Just token -> do
            liftIO $ atomically $ modifyTVar cache (Map.insert token creds)
            addHeader "Location" (LT.pack url)
            status found302
        Nothing -> do
            status notFound404

data AuthResponse = AuthResponse
    { twToken :: String
    , twSecret :: String
    } deriving (Eq, Show)

instance ToJSON AuthResponse where
    toJSON (AuthResponse token secret) = object ["token" .= token, "secret" .= secret]


callbackHadler :: CredentialCache -> ActionM ()
callbackHadler cache = do
    token <- param "oauth_token"
    verifier <- param "oauth_verifier"

    storedCreds <- liftIO $ atomically $ do
        m <- readTVar cache
        let rq = Map.lookup token m
        modifyTVar cache $ Map.delete token
        return rq

    case storedCreds of
        Nothing -> status notFound404
        Just creds -> do
            accessToken <- liftIO $ requestToAccess creds verifier
            liftIO $ print accessToken
            let token  = BS.unpack <$> lookup "oauth_token" (unCredential accessToken)
                secret = BS.unpack <$> lookup "oauth_token_secret" (unCredential accessToken)
                authResponseMb = AuthResponse <$> token <*> secret
            json authResponseMb

tw_main :: IO ()
tw_main = do
    cache <- newTVarIO Map.empty
    scotty 8080 $ do
        get "/"         $ redirect "signin"
        get "/signin"   $ signinHandler cache
        get "/callback" $ callbackHadler cache
        get "/main"     $ text "main page"
        get "/inspect"  $ inspect

inspect :: ActionM ()
inspect = do
    hs <- headers
    liftIO $ forM_ hs $ \(h, v) -> (LT.putStrLn $ h <> " : " <> v)
    return ()