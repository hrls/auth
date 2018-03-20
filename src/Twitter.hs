{-# LANGUAGE OverloadedStrings #-}
module Twitter where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as LT
import Data.Aeson hiding (json)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM

import Control.Monad.Trans.Reader (ReaderT, asks)
import Control.Monad.Trans.Class (lift)

import Network.HTTP.Client (Manager)
import Web.Authenticate.OAuth
import Network.HTTP.Types.Status
import Web.Scotty


type ApiKey    = String
type ApiSecret = String

srvOAuth :: (ApiKey, ApiSecret) -> OAuth
srvOAuth (apiKey, apiSecret) = newOAuth
    { oauthRequestUri = "https://api.twitter.com/oauth/request_token"
    , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
    , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
    , oauthConsumerKey = BS.pack apiKey
    , oauthConsumerSecret = BS.pack apiSecret
    }


type TokenKey = ByteString
type CredsCache = TVar (Map TokenKey Credential)

mkCache :: IO CredsCache
mkCache = newTVarIO Map.empty


data AppEnv = AppEnv
    { tokenAndSecret :: (ApiKey, ApiSecret)
    , credsCache :: CredsCache
    , httpManager :: Manager
    }


type AppHttpAction = ReaderT AppEnv ActionM


signinHandler :: AppHttpAction ()
signinHandler = do
    originHostMb <- lift $ header "Host"
    apiPair <- asks tokenAndSecret
    let originCallbackUrl = (\h -> "http://" ++ (LT.unpack h) ++ "/callback") <$> originHostMb
        modSrvOAuth = (srvOAuth apiPair) { oauthCallback = BS.pack <$> originCallbackUrl }

    mngr <- asks httpManager
    tmp <- lift $ liftAndCatchIO $ getTemporaryCredential modSrvOAuth mngr

    case lookup "oauth_token" (unCredential tmp) of
        Just token -> do
            cache <- asks credsCache
            lift $ liftAndCatchIO $ atomically $ modifyTVar cache (Map.insert token tmp)
            lift $ addHeader "Location" (LT.pack $ authorizeUrl modSrvOAuth tmp) >> status seeOther303
        Nothing -> do
            lift $ status serviceUnavailable503 -- todo: check twitter response and do something


data AuthResponse = AuthResponse
    { twToken :: String
    , twSecret :: String
    } deriving (Eq, Show)

instance ToJSON AuthResponse where
    toJSON (AuthResponse token secret) = object ["token" .= token, "secret" .= secret]


callbackHadler :: AppHttpAction ()
callbackHadler = do
    token    <- lift $ param "oauth_token"
    verifier <- lift $ param "oauth_verifier"

    cache <- asks credsCache
    storedCreds <- lift $ liftAndCatchIO $ atomically $ do
        m <- readTVar cache
        let rq = Map.lookup token m
        modifyTVar cache $ Map.delete token
        return rq

    case storedCreds of
        Nothing -> lift $ status notFound404
        Just creds -> do
            let verified = injectVerifier verifier creds
            apiPair <- asks tokenAndSecret
            mngr <- asks httpManager
            accessToken <- lift $ liftAndCatchIO $ getAccessToken (srvOAuth apiPair) verified mngr

            let token  = BS.unpack <$> lookup "oauth_token" (unCredential accessToken)
                secret = BS.unpack <$> lookup "oauth_token_secret" (unCredential accessToken)
                authResponseMb = AuthResponse <$> token <*> secret
            lift $ json authResponseMb

