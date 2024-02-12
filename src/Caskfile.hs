module Caskfile (getFileIdFromPath, listCaskFiles, removePrevFiles, getLastFileId, getCurrentFileId, prependEntry, readEntryFromPos, createCaskLock, readEntries) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Entry (Entry (..), getEntryLength)
import Serializable (decode, encode)
import System.Directory (doesFileExist, listDirectory, removeFile)
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))

getFileIdFromPath :: FilePath -> Int
getFileIdFromPath = read . takeBaseName

listCaskFiles :: FilePath -> IO [FilePath]
listCaskFiles dirpath = do
  workfiles <- listDirectory dirpath
  case filter (\workfile -> takeExtension workfile == ".cask") workfiles of
    [] -> pure []
    files -> pure files

removePrevFiles :: FilePath -> IO ()
removePrevFiles filepath = do
  let fileid = getFileIdFromPath filepath
  let dir = takeDirectory filepath
  caskfiles <- listCaskFiles dir
  let casks = map getFileIdFromPath caskfiles
  let caskIdsToRemove = filter (< fileid) casks
  let caskfilesToRemove = map (\fileid' -> dir </> show fileid' ++ ".cask") caskIdsToRemove
  mapM_ removeFile caskfilesToRemove

getLastFileId :: FilePath -> IO Int
getLastFileId dirpath = do
  caskfiles <- listCaskFiles dirpath
  let fileIds = map getFileIdFromPath caskfiles
  case fileIds of
    [] -> pure 0
    _ -> pure $ maximum fileIds

getCurrentFileId :: FilePath -> IO Int
getCurrentFileId dirpath = fmap (1 +) (getLastFileId dirpath)

prependEntry :: FilePath -> Entry -> IO Entry
prependEntry filepath entry = do
  exists <- doesFileExist filepath
  contents <- if exists then B.readFile filepath else pure B.empty
  BL.writeFile filepath $ encode entry <> BL.fromStrict contents
  pure entry

decodeWithOffset :: BL.ByteString -> Int -> IO [(Int, Entry)]
decodeWithOffset content offset = do
  let content' = BL.drop (fromIntegral offset) content
  if content' == BL.empty
    then do pure []
    else do
      let entry = decode content'
      entries <- decodeWithOffset content $ offset + getEntryLength entry
      pure $ (offset, entry) : entries

readEntries :: FilePath -> IO [(Int, Entry)]
readEntries filepath = do
  content <- BL.readFile filepath
  decodeWithOffset content 0

readEntryFromPos :: FilePath -> Int64 -> Int64 -> Int64 -> IO Entry
readEntryFromPos filepath ksize vsize offset = do
  let offset' = offset - vsize - ksize - 8 - 8 - 8 - 4
  content <- BL.readFile filepath
  pure $ decode $ BL.drop offset' content

createCaskLock :: FilePath -> IO ()
createCaskLock dirpath = do
  BL.writeFile (dirpath </> "cask.lock") ""
