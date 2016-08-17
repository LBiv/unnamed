{-# LANGUAGE OverloadedStrings #-}

module DataId
    ( DataBodyIdStr
    , KeyslotIdStr
    , DataIdStr(..)
    , dataIdStr
    , dataId
    , dataBodyId
    , keyslotId
    )  where

import Data.Text.Encoding
import qualified Data.ByteString as B 
import Data.Aeson.Types
import Data.Aeson
import GHC.Exts
import qualified Data.Vector as V
import qualified Data.Int as I


type DataBodyIdStr = B.ByteString
type KeyslotIdStr = B.ByteString

newtype DataIdStr = DataIdStr (DataBodyIdStr, [KeyslotIdStr])
  deriving Show

dataId :: I.Int64 -> [I.Int64] -> DataIdStr
dataId a b = DataIdStr ((encodeUtf8. fromList . show) a, fmap (encodeUtf8 . fromList . show) b)

dataIdStr :: DataBodyIdStr -> [KeyslotIdStr] -> DataIdStr
dataIdStr a b = DataIdStr (a, b)

dataBodyId :: DataIdStr -> I.Int64
dataBodyId (DataIdStr (a, _)) = (read . toList . decodeUtf8) a

keyslotId :: DataIdStr -> [I.Int64]
keyslotId  (DataIdStr (_, as)) = fmap (read . toList . decodeUtf8) as

instance ToJSON DataIdStr where
  toJSON (DataIdStr (body, as)) = object [("dataId", String (decodeUtf8 body)), ("keyslotIds", Array $ fromList $ fmap (String . decodeUtf8) as)]

instance FromJSON DataIdStr where
  parseJSON (Object v) = do
    body <- fmap encodeUtf8 $ v .: "dataId"
    keyslots <- (fmap ((V.toList) . (fmap encodeUtf8))  $ v .: "keyslotIds") :: Parser [KeyslotIdStr]
    return $ DataIdStr (body, keyslots)
