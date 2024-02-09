module Caskfile where

import qualified Data.ByteString.Lazy as B

import System.FilePath (takeBaseName, takeExtension)
import System.Directory (getCurrentDirectory, listDirectory, doesFileExist)

import Serializable (encode, decode)
import Entry (Entry (..), getEntryLength)

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
    B.writeFile path $ (encode entry) <> contents
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
readEntries path = do
    content <- B.readFile $ "temp/" ++ path
    decodeWithOffset content 0
