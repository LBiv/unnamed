{-# LANGUAGE OverloadedStrings #-}

module Apps
    ( providerTokenApp
    , providerInfoApp
    , loginApp
    , userTokenRetrieveApp 
    , invalidApp
    , storeDataApp
    , loadDataApp
    ) where


import Data.ByteString.Builder (toLazyByteString)
import Data.Aeson.Types
import Data.Aeson
import Provider
import User
import DataObject
import DataId
import qualified DataAccess as DA
import Network.Wai
import Network.HTTP.Types
import Network.HTTP.Types.Method
import Control.Monad
import           Control.Monad.IO.Class  (liftIO)
import Database.Persist.Sql (ConnectionPool)
import qualified Data.ByteString as B


providerTokenApp :: Maybe ProviderToken -> Application
providerTokenApp tokenM a respond = do
    putStrLn "Provider Info App"
    print a
    case tokenM of 
        Just token  ->
            respond $ responseLBS
            status200
            (("Content-Type", "application/json") : corsHeader)
            $ toLazyByteString $ fromEncoding $ toEncoding $ toJSON token
        _ -> invalidApp a respond
          

providerInfoApp :: Maybe ProviderInfo -> Application
providerInfoApp infoM a respond = do
    putStrLn "Provider Info App"
    print a
    case infoM of
        Just info ->
            respond $ responseLBS
            status200
            (("Content-Type", "application/json") : corsHeader)
            $ toLazyByteString $ fromEncoding $ toEncoding $ toJSON info
        _ -> invalidApp a respond


userTokenRetrieveApp :: Application
userTokenRetrieveApp a respond = do
    putStrLn "User token retrieve app"
    print a
    let rmethod = requestMethod a
    if rmethod == methodPost then
        userTokenRetrievePostApp a respond
    else if rmethod == methodOptions then
        optionsApp a respond 
    else 
        invalidApp a respond 


userTokenRetrievePostApp :: Application
userTokenRetrievePostApp a respond = do
    putStrLn "User token post retrieve app"
    body <- requestBody a
    case parseUserTokenRetrieve body of 
        Just iden -> do
            internalIdM <- getInternalUserId iden
            case internalIdM of 
                Just intern -> do
                    tokenM <- loadUserToken intern
                    case tokenM of 
                        Just tok -> do 
                            respond ( responseLBS
                                status200
                                (("Content-Type", "application/json") : corsHeader)
                                (( toLazyByteString . fromEncoding . toEncoding . toJSON) tok) )
                        _        -> invalidApp a respond
                _           -> invalidApp a respond
        _         -> invalidApp a respond


storeDataApp :: ConnectionPool -> Application
storeDataApp pool a respond = do
  putStrLn "Store data app"
  print a
  let rmethod = requestMethod a
  if rmethod == methodPost then
    storeDataPostApp pool a respond
  else if rmethod == methodOptions then
    optionsApp a respond
  else
    invalidApp a respond

storeDataPostApp :: ConnectionPool -> Application
storeDataPostApp pool a respond = do
  putStrLn "Store data post app"
  body <- requestBody a
  let tostore = decodeStrict body :: Maybe DataObject
  let ioidsM = ((DA.dataAccessStoreData pool) <$> tostore)
  case ioidsM of
    Just ioids -> do
      ids <- ioids
      respond $ responseLBS
        status200
        (("Content-Type", "application/json") : corsHeader)
        ((toLazyByteString . fromEncoding . toEncoding . toJSON) ids) 
    _ -> invalidApp a respond

loadDataApp :: ConnectionPool -> Application
loadDataApp pool a respond = do
  putStrLn "Load data app"
  print a
  let rmethod = requestMethod a
  if rmethod == methodPost then
    loadDataPostApp pool a respond
  else if rmethod == methodOptions then
    optionsApp a respond
  else
    invalidApp a respond

loadDataPostApp :: ConnectionPool -> Application
loadDataPostApp pool a respond = do
  putStrLn "Load data post app"
  body <- requestBody a
  print body
  let iofetchedM = (DA.dataAccessLoadData pool) <$> (decodeStrict body)
  case iofetchedM of
    Just iofetched -> do
      fetched <- iofetched
      respond $ responseLBS
        status200
        (("Content-Type", "application/json") : corsHeader)
        ((toLazyByteString . fromEncoding . toEncoding . toJSON) fetched) 
    _ -> invalidApp a respond
        


loginApp :: Application
loginApp a respond = do
    putStrLn "Login App"
    print a
    body <-  requestBody a
    let parsed = parseLogin body
    print parsed
    case parsed of 
       "OK" ->  respond $ responseLBS
                    status200
                    (("Content-Type", "text/plain") : corsHeader)
                    "Hello, web!"
       _    ->  respond $ responseLBS
                    status500
                    (("Content-Type", "text/plain") : corsHeader)
                    "error"


mainApp :: Application
mainApp a respond = do
    putStrLn "I've done some IO here"
    print a
    respond $ responseLBS
        status200
        (("Content-Type", "text/plain") : corsHeader)
        "Hello, web!"


invalidApp :: Application
invalidApp a respond = do
    putStrLn "invalid request"
    respond $ responseLBS
        status400
        (("Content-Type", "text/plain") : corsHeader)
        "Invalid Request"

optionsApp :: Application
optionsApp a respond = do
    putStrLn "Options request"
    respond $ responseLBS
        status200
        (("Content-Type", "text/plain") : corsHeader)
        "options response"


parseLogin :: B.ByteString -> B.ByteString
parseLogin _ = "OK" 


corsHeader = [
  ("Access-Control-Allow-Origin", "*"),
  ("Access-Control-Allow-Method", "GET, POST"),
  ("Access-Control-Allow-Headers", "Content-Type")]
