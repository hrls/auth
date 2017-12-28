module Main where

import Network.HTTP.Client.TLS

import Twitter.Api (tw_main)

main :: IO ()
main = do
    putStrLn "auth server"
    tlsManager <- newTlsManager
    setGlobalManager tlsManager
    tw_main
