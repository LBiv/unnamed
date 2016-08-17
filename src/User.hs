{-# LANGUAGE OverloadedStrings #-}

module User
    ( UserIdentifier(..)
    , InternalUserId(..)
    , UserToken(..)
    , parseUserTokenRetrieve
    , loadUserToken
    , getInternalUserId
    ) where

import Data.Text.Encoding
import qualified Data.ByteString as B 
import Data.Aeson
import Data.Aeson.Types


newtype UserIdentifier = UserIdentifier { userIdentifierString :: B.ByteString }

instance FromJSON UserIdentifier where
    parseJSON (Object v) = UserIdentifier <$> encodeUtf8 <$> v .: "UserIdentifier"


newtype InternalUserId = InternalUserId { internalUserId :: Integer }


newtype UserToken = UserToken { userTokenString :: B.ByteString }

instance ToJSON UserToken where
    toJSON a = object ["UserToken" .= (decodeUtf8 (userTokenString a)) ]



parseUserTokenRetrieve :: B.ByteString -> Maybe UserIdentifier
parseUserTokenRetrieve = decodeStrict

getInternalUserId :: UserIdentifier -> IO (Maybe InternalUserId)
getInternalUserId a  = do
    return $ Just $ InternalUserId 12412

loadUserToken :: InternalUserId -> IO (Maybe UserToken)
loadUserToken a = do
    return $ Just $ UserToken "usertoken placeholder"

