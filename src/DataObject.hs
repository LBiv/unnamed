{-# LANGUAGE OverloadedStrings #-}

module DataObject
    ( DataBody
    , Keyslot
    , DataObject(..)
    , dataObject
    )  where

import Data.Text.Encoding
import qualified Data.ByteString as B 
import Data.Aeson.Types
import Data.Aeson
import GHC.Exts
import qualified Data.Vector as V


type DataBody = B.ByteString
type Keyslot = B.ByteString

newtype DataObject = DataObject (DataBody, [Keyslot])
  deriving Show

dataObject :: DataBody -> [Keyslot] -> DataObject
dataObject a b = DataObject (a, b)

instance ToJSON DataObject where
  toJSON (DataObject (body, as)) = object [("data", String (decodeUtf8 body)), ("keyslots", Array $ fromList $ fmap (String . decodeUtf8) as)]

instance FromJSON DataObject where
  parseJSON (Object v) = do
    body <- fmap encodeUtf8 $ v .: "data"
    keyslots <- (fmap ((V.toList) . (fmap encodeUtf8))  $ v .: "keyslots") :: Parser [Keyslot]
    return $ DataObject (body, keyslots)
