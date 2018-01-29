{-# LANGUAGE OverloadedStrings #-}
module Twitter.Auth where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Network.HTTP.Client.TLS (getGlobalManager)
import Web.Authenticate.OAuth

type ApiKey = String
type ApiSecret = String

srvOAuth :: (ApiKey, ApiSecret) -> OAuth
srvOAuth ks = newOAuth
    { oauthRequestUri = "https://api.twitter.com/oauth/request_token"
    , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
    , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
    , oauthConsumerKey = BS.pack (fst ks)
    , oauthConsumerSecret = BS.pack (snd ks)
    }

-- | https://dev.twitter.com/web/sign-in/implementing

type Token = ByteString
type Verifier = ByteString
type Url = String


oauthToken :: Credential -> Maybe Token
oauthToken creds = lookup "oauth_token" (unCredential creds)

requestToken :: OAuth -> IO Credential
requestToken auth = do
    getTemporaryCredential auth =<< getGlobalManager

obtainRequestToken :: (ApiKey, ApiSecret) -> Maybe Url -> IO (Credential, Url)
obtainRequestToken keySecret callback = do
    let modSrvOAuth = (srvOAuth keySecret) { oauthCallback = BS.pack <$> callback }
    token <- requestToken modSrvOAuth
    return (token, authorizeUrl modSrvOAuth token)

requestToAccess :: (ApiKey, ApiSecret) -> Credential -> Verifier -> IO Credential
requestToAccess keySecret requestToken verifier = do
    let verified = injectVerifier verifier requestToken
    getAccessToken (srvOAuth keySecret) verified =<< getGlobalManager
