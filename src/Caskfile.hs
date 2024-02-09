module Caskfile where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BU

import Data.Binary.Put (putInt64le, putWord32le, putLazyByteString, runPut)
import Data.Binary.Get (getInt64le, getWord32le, getLazyByteString, runGet, Get)

import System.FilePath (takeBaseName, takeExtension)
import System.Directory (getCurrentDirectory, listDirectory, doesFileExist)

import Entry (Entry (..), getEntryLength)

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

listCaskFiles :: IO [FilePath]
listCaskFiles = do
    currentdir <- getCurrentDirectory
    let workdir = currentdir ++ "/temp"
    workfiles <- listDirectory workdir
    case filter (\workfile -> takeExtension workfile == ".cask") workfiles of
        [] -> pure []
        files -> pure files

getLastFileId :: IO Int
getLastFileId = do
    caskfiles <- listCaskFiles
    let filenames = map (read . takeBaseName) caskfiles
    case filenames of
        [] -> pure 0
        _ -> pure $ maximum filenames

getCurrentFileId :: IO Int
getCurrentFileId = fmap (1+) getLastFileId

prependEntry :: Int -> Entry -> IO Entry
prependEntry fileid entry = do
    let path = "temp/" ++ show fileid ++ ".cask"
    exists <- doesFileExist path
    !contents <- if exists then B.readFile path else pure B.empty
    print contents
    B.writeFile path $ (serialiseEntry entry) <> contents
    pure entry

readEntry :: B.ByteString -> IO Entry
readEntry content = do
    pure $ runGet deserialiseEntry content

deserialiseWithOffset :: B.ByteString -> Int -> IO [(Int, Entry)]
deserialiseWithOffset content offset = do
    let content' = B.drop (fromIntegral offset) content
    if content' == B.empty
    then do pure []
    else do
        entry <- readEntry content'
        entries <- deserialiseWithOffset content $ offset + (getEntryLength entry)
        pure $ (offset, entry) : entries

readEntries :: FilePath -> IO [(Int, Entry)]
readEntries path = do
    content <- B.readFile $ "temp/" ++ path
    deserialiseWithOffset content 0
