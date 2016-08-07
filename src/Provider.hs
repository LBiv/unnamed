{-# LANGUAGE OverloadedStrings #-}

module Provider
    ( ProviderToken(..)
    , ProviderInfo(..)
    ) where

import Data.Text.Encoding
import qualified Data.ByteString as B 
import Data.Aeson.Types
import Database.Persist

data ProviderToken = ProviderToken { providerTokenString :: B.ByteString }

instance ToJSON ProviderToken where
    toJSON a = object ["ProviderToken" .= (decodeUtf8 (providerTokenString a)) ]


data ProviderInfo = ProviderInfo { providerAddressString :: B.ByteString }

instance ToJSON ProviderInfo where
    toJSON a = object ["address" .= (decodeUtf8 (providerAddressString a)) ]



