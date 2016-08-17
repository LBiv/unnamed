{-# LANGUAGE OverloadedStrings #-}

module Routes
    ( appRoutes
    ) where

import Network.Wai
import Network.Wai.UrlMap
import Control.Applicative
import Provider
import DataAccess
import Database.Persist.Sql
import Apps ( providerTokenApp
            , providerInfoApp
            , userTokenRetrieveApp
            , storeDataApp
            , loadDataApp
            , loginApp
            , invalidApp
            )


appRoutes :: ConnectionPool -> IO Application
appRoutes dbConnPool = do
    tokBS <- dataAccessProviderToken dbConnPool
    infBS <- dataAccessProviderInfo dbConnPool 
    return $ mapUrls $
      mount "id" (providerTokenApp (fmap ProviderToken tokBS)) <|>
      mount "info" (providerInfoApp (fmap ProviderInfo infBS)) <|>
      mount "storeData" (storeDataApp dbConnPool) <|>
      mount "loadData" (loadDataApp dbConnPool) <|> 
      mount "login" loginApp <|>
      mount "userToken" userTokenRetrieveApp <|>
      mountRoot invalidApp
