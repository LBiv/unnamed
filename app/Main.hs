{-# LANGUAGE OverloadedStrings #-}

module Main where

import User
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.UrlMap
import Network.HTTP.Types
import Control.Applicative

app :: Application
app a respond = do
    putStrLn "I've done some IO here"
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain"), ("Access-Control-Allow-Origin", "*")]
        "Hello, web!"


authenticate :: Middleware
authenticate app = app


providerIdApp :: Application
providerIdApp a respond = do
    putStrLn "provider Id token Requested"
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain"), ("Access-Control-Allow-Origin", "*")]
        "Provider Id token"


providerInfoApp :: Application
providerInfoApp a respond = do
    putStrLn "provider info Requested"
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain"), ("Access-Control-Allow-Origin", "*")]


        "Provider info"

fullApp :: Application
fullApp = mapUrls $
    mount "id" providerIdApp <|>
    mount "info" providerInfoApp <|>
    mount "login" app <|>
    mountRoot app

-- router :: Middleware
-- router a =


main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 fullApp
