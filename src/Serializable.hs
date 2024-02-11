module Serializable (encode, decode) where

import qualified Data.ByteString.Lazy as B

import Data.Binary.Put (putInt64le, putWord32le, putLazyByteString, runPut)
import Data.Binary.Get (getInt64le, getWord32le, getLazyByteString, runGet)

import Entry (Entry (..))

class Serializable a where
    encode :: a -> B.ByteString
    decode :: B.ByteString -> a

instance Serializable Entry where
    encode (Entry checksum timestamp ksize vsize key value) =
        runPut $ putWord32le checksum <> putInt64le timestamp <> putInt64le ksize <> putInt64le vsize <> putLazyByteString key <> putLazyByteString value
    decode content = Entry checksum timestamp ksize vsize key value
        where
            value = runGet (getLazyByteString vsize) (B.drop (4 + 8 + 8 + 8 + ksize) content)
            key = runGet (getLazyByteString ksize) (B.drop (4 + 8 + 8 + 8) content)
            vsize = runGet getInt64le (B.drop (4 + 8 + 8) content)
            ksize = runGet getInt64le (B.drop (4 + 8) content)
            timestamp = runGet getInt64le (B.drop 4 content)
            checksum = runGet getWord32le content
