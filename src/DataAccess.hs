{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module DataAccess where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (LoggingT, runStderrLoggingT)
import           Control.Monad
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Class
import           Database.Persist.TH
import           Data.Aeson.Types
import           Data.ByteString


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ProviderData
    token ByteString
    info ByteString
    deriving Show
|]


connStr = "host=localhost user=unnamed port=5432 dbname=postgres"

dbConnPool :: LoggingT IO ConnectionPool
dbConnPool = createPostgresqlPool connStr 10
  
dataAccessProviderToken :: ConnectionPool -> IO (Maybe ByteString)
dataAccessProviderToken pool = runStderrLoggingT $ liftIO $ do
     (flip runSqlPersistMPool) pool $ do
          row <- get $ ProviderDataKey 1
          return $ fmap providerDataToken row


dataAccessProviderInfo :: ConnectionPool -> IO (Maybe ByteString)
dataAccessProviderInfo pool = runStderrLoggingT $ liftIO $ do
    (flip runSqlPersistMPool) pool $ do
        row <- get $ ProviderDataKey 1
        return $ fmap providerDataInfo row


