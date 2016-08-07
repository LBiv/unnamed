{-# LANGUAGE OverloadedStrings #-}

module Main where

import Routes
import DataAccess
import Network.Wai.Handler.Warp (run)
import Control.Monad.Logger    (runStderrLoggingT)
import Control.Monad.IO.Class  (liftIO)

main :: IO ()
main = do 
    dbConnectionPool <- runStderrLoggingT dbConnPool
    application <- (appRoutes dbConnectionPool)
    putStrLn $ "http://localhost:8080/"
    run 8080 application
