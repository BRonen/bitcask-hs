module Entry (nanosSinceEpoch, buildEntry, getEntryLength) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BU

import Data.Digest.CRC32 (crc32)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32)

type Checksum = Word32
type Timestamp = Int64
type FieldSize = Int64
type Key = B.ByteString
type Value = B.ByteString

data Entry = Entry Checksum Timestamp FieldSize FieldSize Key Value
    deriving (Eq)

instance Show Entry where
  show (Entry fileid timestamp ksize vsize key value) =
    [i|#{fileid'} #{timestamp'} #{ksize'} #{vsize'} #{key'} #{value'}|]
    where
        fileid' = show fileid
        timestamp' = show timestamp
        ksize' = show ksize
        vsize' = show vsize
        key' = BU.toString key
        value' = BU.toString value

nanosSinceEpoch :: IO Timestamp
nanosSinceEpoch = do
    t <- getPOSIXTime
    pure $ floor $ 1e9 * nominalDiffTimeToSeconds t

buildEntry :: Timestamp -> Key -> Value -> Entry
buildEntry timestamp key value = Entry checksum timestamp keyl valuel key value
    where
        checksum = crc32 $ BU.fromString $ show timestamp ++ show keyl ++ show valuel ++ key' ++ value'
        keyl = B.length key
        valuel = B.length value
        key' = BU.toString key
        value' = BU.toString value

getEntryLength :: Entry -> Int
getEntryLength (Entry _ _ ksize vsize _ _) = fromIntegral $ 4 + 8 + 8 + 8 + ksize + vsize
