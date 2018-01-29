module Main where

import System.Environment (lookupEnv)
import Network.HTTP.Client.TLS

import Twitter.Api (tw_main)

main :: IO ()
main = do
    putStrLn "auth server"

    tlsManager <- newTlsManager
    setGlobalManager tlsManager

    Just token  <- lookupEnv "TWITTER_API_KEY"
    Just secret <- lookupEnv "TWITTER_API_SECRET"

    tw_main token secret
