module Caskfile where

import qualified Data.ByteString.Lazy as B

import Data.Int (Int64)
import Data.Binary.Get (getLazyByteString, runGet)
import System.FilePath (takeBaseName, takeExtension)
import System.Directory (getCurrentDirectory, listDirectory, doesFileExist)

import Serializable (encode, decode)
import Entry (Entry (..), Value, getEntryLength)

getFileIdFromPath :: FilePath -> Int
getFileIdFromPath = read . takeBaseName

listCaskFiles :: FilePath -> IO [FilePath]
listCaskFiles dirpath = do
    workfiles <- listDirectory dirpath
    case filter (\workfile -> takeExtension workfile == ".cask") workfiles of
        [] -> pure []
        files -> pure files

getLastFileId :: FilePath -> IO Int
getLastFileId dirpath = do
    caskfiles <- listCaskFiles dirpath
    let fileIds = map getFileIdFromPath caskfiles
    case fileIds of
        [] -> pure 0
        _ -> pure $ maximum fileIds

getCurrentFileId :: FilePath -> IO Int
getCurrentFileId dirpath = fmap (1+) (getLastFileId dirpath)

prependEntry :: FilePath -> Entry -> IO Entry
prependEntry filepath entry = do
    exists <- doesFileExist filepath
    !contents <- if exists then B.readFile filepath else pure B.empty
    print contents
    B.writeFile filepath $ (encode entry) <> contents
    pure entry

decodeWithOffset :: B.ByteString -> Int -> IO [(Int, Entry)]
decodeWithOffset content offset = do
    let content' = B.drop (fromIntegral offset) content
    if content' == B.empty
    then do pure []
    else do
        let entry = decode content'
        entries <- decodeWithOffset content $ offset + (getEntryLength entry)
        pure $ (offset, entry) : entries

readEntries :: FilePath -> IO [(Int, Entry)]
readEntries filepath = do
    content <- B.readFile filepath
    decodeWithOffset content 0

readValueFromPos :: FilePath -> Int64 -> Int64 -> IO Value
readValueFromPos filepath vsize offset = do
    content <- B.readFile filepath
    pure $ runGet (getLazyByteString vsize) (B.drop offset content)

createCaskLock :: FilePath -> IO ()
createCaskLock dirpath = do
    B.writeFile (dirpath ++ "cask.lock") ""
