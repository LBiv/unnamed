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
import qualified DataObject as DO
import qualified DataId as DI


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ProviderData
    token ByteString
    info ByteString
    deriving Show
DataStorage
    data ByteString
    deriving Show
Keyslot
    dataStorageId DataStorageId
    data ByteString
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


dataAccessStoreData :: ConnectionPool -> DO.DataObject -> IO DI.DataIdStr
dataAccessStoreData pool (DO.DataObject (dataObj, keyslots)) = runStderrLoggingT $ liftIO $ do
  (flip runSqlPersistMPool) pool $ do
    newdataId <- insert $ DataStorage dataObj
    keyslotIds <- mapM insert ((fmap (Keyslot newdataId) keyslots))
    return (DI.dataId (fromSqlKey newdataId) (fmap fromSqlKey keyslotIds))

dataAccessLoadData :: ConnectionPool -> DI.DataIdStr -> IO (Maybe DO.DataObject)
dataAccessLoadData pool idstr = runStderrLoggingT $ liftIO $ do
  (flip runSqlPersistMPool) pool $ do
    fetcheddata <- get $ ((toSqlKey $ DI.dataBodyId idstr) :: DataStorageId)
    fetchedslots <- selectList
      (Prelude.foldr1 (\x -> (\y -> (x  ||. y)))
        (fmap
          (\x -> [KeyslotId ==. (toSqlKey x), KeyslotDataStorageId ==. (toSqlKey (DI.dataBodyId idstr))])
          (DI.keyslotId idstr)))
      []
    return $ DO.dataObject <$> (fmap dataStorageData fetcheddata) <*> (pure (fmap (keyslotData . entityVal) fetchedslots))


