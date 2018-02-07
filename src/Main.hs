{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment         (lookupEnv)
import Network.HTTP.Client.TLS    (newTlsManager)
import Control.Monad.Trans.Reader (runReaderT)
import Web.Scotty                 (scotty, get, notFound, redirect)

import Twitter

main :: IO ()
main = do
    putStrLn "auth server"

    Just apiKey    <- lookupEnv "TWITTER_API_KEY"
    Just apiSecret <- lookupEnv "TWITTER_API_SECRET"
    cache          <- mkCache
    tlsManager     <- newTlsManager

    let appEnv = AppEnv {
        tokenAndSecret = (apiKey, apiSecret),
        credsCache     = cache,
        httpManager    = tlsManager
    }

    scotty 5000 $ do
        get "/signin"   $ runReaderT signinHandler  appEnv
        get "/callback" $ runReaderT callbackHadler appEnv
        notFound        $ redirect "/signin"

