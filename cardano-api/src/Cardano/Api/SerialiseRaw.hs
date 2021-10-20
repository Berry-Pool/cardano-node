{-# LANGUAGE ScopedTypeVariables #-}

-- | Raw binary serialisation
--
module Cardano.Api.SerialiseRaw
  ( SerialiseAsRawBytes(..)
  , serialiseToRawBytesHex
  , deserialiseFromRawBytesHex
  , serialiseToRawBytesHexText
  ) where

import           Cardano.Prelude
import           Prelude (String)

import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.Encoding as Text

import           Cardano.Api.HasTypeProxy


class HasTypeProxy a => SerialiseAsRawBytes a where

  serialiseToRawBytes :: a -> ByteString

  deserialiseFromRawBytes :: AsType a -> ByteString -> Maybe a

serialiseToRawBytesHex :: SerialiseAsRawBytes a => a -> ByteString
serialiseToRawBytesHex = Base16.encode . serialiseToRawBytes

serialiseToRawBytesHexText :: SerialiseAsRawBytes a => a -> Text
serialiseToRawBytesHexText = Text.decodeUtf8 . serialiseToRawBytesHex

deserialiseFromRawBytesHex :: SerialiseAsRawBytes a
                           => AsType a -> ByteString -> Either String a
deserialiseFromRawBytesHex proxy hex = do
  raw <- Base16.decode hex
  note ("Cannot deserialise " ++ show hex) $ deserialiseFromRawBytes proxy raw
