{-# LANGUAGE OverloadedStrings #-}

module StorageApp
  where


import Data.ByteString.Builder (toLazyByteString)
import Data.Aeson.Types
import Network.Wai
import Network.HTTP.Types
import Network.HTTP.Types.Method
import Control.Monad
import Persistent.Sql
import qualified Data.ByteString as B


storageStore :: ConnectionPool -> Application
storageStore dbpool a respond = do
    putStrLn "Storage application: store"
    print a
    let rmethod = requestMethod a
    if rmethod = methodPost then
       
    case tokenM of 
        Just token  ->
            respond $ responseLBS
            status200
            (("Content-Type", "application/json") : corsHeader)
            $ toLazyByteString $ fromEncoding $ toEncoding $ toJSON token
        _ -> invalidApp a respond
          

storageLoad :: ConnectionPool -> Application
storageLoad dbpool a respond = do
    putStrLn "Storage application: load"
    print a
    case infoM of
        Just info ->
            respond $ responseLBS
            status200
            (("Content-Type", "application/json") : corsHeader)
            $ toLazyByteString $ fromEncoding $ toEncoding $ toJSON info
        _ -> invalidApp a respond




corsHeader = [
  ("Access-Control-Allow-Origin", "*"),
  ("Access-Control-Allow-Method", "GET, POST"),
  ("Access-Control-Allow-Headers", "Content-Type")]
