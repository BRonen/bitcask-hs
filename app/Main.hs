module Main (main) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BU
import qualified Data.Map as Map

import Data.Binary.Put (putInt64le, putWord32le, putLazyByteString, runPut)
import Data.Binary.Get (getInt64le, getWord32le, getLazyByteString, runGet, Get)
import Data.Digest.CRC32 (crc32)
import Data.Int (Int64)
import Data.String.Interpolate
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32)

import System.FilePath (takeBaseName, takeExtension)
import System.Directory (getCurrentDirectory, listDirectory, doesFileExist)

data Entry = Entry Word32 Int64 Int64 Int64 B.ByteString B.ByteString
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

serialiseEntry :: Entry -> BU.ByteString
serialiseEntry (Entry checksum timestamp ksize vsize key value) =
    runPut $ putWord32le checksum <> putInt64le timestamp <> putInt64le ksize <> putInt64le vsize <> putLazyByteString key <> putLazyByteString value

deserialiseEntry :: Get Entry
deserialiseEntry = do
    checksum <- getWord32le
    timestamp <- getInt64le
    ksize <- getInt64le
    vsize <- getInt64le
    key <- getLazyByteString ksize
    value <- getLazyByteString vsize
    pure $ Entry checksum timestamp ksize vsize key value

data KeydirEntry = KeydirEntry Int Int Int Int64
    deriving (Show, Eq)

type Keydir = Map.Map B.ByteString KeydirEntry

nanosSinceEpoch :: IO Int64
nanosSinceEpoch = do
    t <- getPOSIXTime
    pure $ floor $ 1e9 * (nominalDiffTimeToSeconds t)

buildEntry :: Int64 -> String -> String -> Entry
buildEntry timestamp key value = Entry entryid timestamp keyl valuel key' value'
    where
        entryid = crc32 $ BU.fromString $ show timestamp ++ show keyl ++ show valuel ++ key ++ value
        keyl = B.length key' :: Int64
        valuel = B.length value' :: Int64
        key' = BU.fromString key
        value' = BU.fromString value

checkSum :: Word32 -> String -> String -> Bool
checkSum entryid key value = entryid == entryid'
    where
        entryid' = crc32 $ BU.fromString $ key ++ value

getLastFileId :: IO Int
getLastFileId = do
    currentdir <- getCurrentDirectory
    let workdir = currentdir ++ "/temp"
    workfiles <- listDirectory workdir
    let files = filter (\workfile -> takeExtension workfile == ".cask") workfiles
    case files of
        [] -> pure 0
        _ -> pure $ maximum $ map (read . takeBaseName) files

prependEntry :: Int -> Entry -> IO Entry
prependEntry fileid entry = do
    let path = "temp/" ++ show fileid ++ ".cask"
    exists <- doesFileExist path
    !contents <- if exists
        then do
            !contents <- B.readFile path
            print contents
            pure contents
        else do
            pure B.empty
    B.writeFile path $ (serialiseEntry entry) <> contents
    pure entry

getEntryLength :: Entry -> Int
getEntryLength (Entry _ _ ksize vsize _ _) = fromIntegral $ 4 + 8 + 8 + 8 + ksize + vsize

readEntry :: B.ByteString -> IO Entry
readEntry content = do
    pure $ runGet deserialiseEntry content

deserialiseWithOffset :: B.ByteString -> Int -> IO [Entry]
deserialiseWithOffset content offset = do
    let content' = B.drop (fromIntegral offset) content
    if content' == B.empty
    then do pure []
    else do
        entry <- readEntry content'
        entries <- deserialiseWithOffset content $ offset + (getEntryLength entry)
        pure $ entry : entries

readEntries :: Int -> IO [Entry]
readEntries fileid = do
    let path = "temp/" ++ show fileid ++ ".cask"
    content <- B.readFile path
    deserialiseWithOffset content 0

main :: IO ()
main = do
    currentFileid <- fmap (0+) getLastFileId
    t <- nanosSinceEpoch
    a <- prependEntry currentFileid $ buildEntry t "hello" "world"
    print $ show a
    readEntries currentFileid >>= print . show
