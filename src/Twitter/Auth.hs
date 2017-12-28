{-# LANGUAGE OverloadedStrings #-}
module Twitter.Auth where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Network.HTTP.Client.TLS (getGlobalManager)
import Web.Authenticate.OAuth

srvOAuth :: OAuth
srvOAuth = newOAuth
    { oauthRequestUri = "https://api.twitter.com/oauth/request_token"
    , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
    , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
    , oauthConsumerKey = "aqudilISPje3uRkNHXpdh8e06"
    , oauthConsumerSecret = "MIRyTMIQww0FlvHKM9hNpF4nYipjBH9NhY67DxoTSnb9EMWElY"
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

obtainRequestToken :: Maybe Url -> IO (Credential, Url)
obtainRequestToken callback = do
    let modSrvOAuth = srvOAuth { oauthCallback = BS.pack <$> callback }
    token <- requestToken modSrvOAuth
    return (token, authorizeUrl modSrvOAuth token)

requestToAccess :: Credential -> Verifier -> IO Credential
requestToAccess requestToken verifier = do
    let verified = injectVerifier verifier requestToken
    getAccessToken srvOAuth verified =<< getGlobalManager
