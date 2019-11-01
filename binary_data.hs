{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

sampleBytes :: B.ByteString
sampleBytes = "Hello!"

sampleString :: String
sampleString = BC.unpack sampleBytes

bcInt :: BC.ByteString
bcInt = "6"

intInt :: Int
intInt = read (BC.unpack bcInt)

uni :: T.Text
uni = "გამარჯობა!"

uniText :: B.ByteString
uniText = E.encodeUtf8 uni
