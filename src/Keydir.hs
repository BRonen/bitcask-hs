module Keydir (mapEntriesToKeydir, getValueFromKeydir, buildKeyDir, listKeysFromKeydir) where

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B

import System.FilePath ((</>))

import Caskfile (listCaskFiles, readEntries, readEntryFromPos)
import Entry (Entry (..), FieldSize, Timestamp, Key, Value, matchChecksum)

data KeydirEntry = KeydirEntry FilePath FieldSize Int Timestamp
    deriving (Show, Eq)

type Keydir = Map.Map Key KeydirEntry

mapEntriesToKeydir :: FilePath -> [(Int, Entry)] -> Keydir
mapEntriesToKeydir _ [] = Map.empty
mapEntriesToKeydir filepath ((offset, Entry _ timestamp ksize vsize key _):entries) =
    let ksize' = fromIntegral ksize in
    let entry = KeydirEntry filepath vsize (ksize' + 4 + 8 + 8 + 8 + offset) timestamp in
    Map.insert key entry $ mapEntriesToKeydir filepath entries

getValueFromKeydir :: Keydir -> Key -> IO (Maybe Value)
getValueFromKeydir keydir key = do
    let keydir' = Map.lookup key keydir
    case keydir' of
        Just (KeydirEntry filepath vsize offset _) -> do
            entry@(Entry _ _ _ _ _ value) <- readEntryFromPos filepath (B.length key) vsize $ fromIntegral offset
            pure $ if matchChecksum entry then Just value else Nothing
        Nothing -> pure Nothing

compareByTimestamp :: KeydirEntry -> KeydirEntry -> KeydirEntry
compareByTimestamp left@(KeydirEntry _ _ _ timestamp) right@(KeydirEntry _ _ _ timestamp') =
    if timestamp > timestamp' then left else right

buildKeyDir :: FilePath -> IO Keydir
buildKeyDir dirpath = do
    caskfiles <- listCaskFiles dirpath
    keydirs <- mapM (\caskfile -> do
        let caskpath = dirpath </> caskfile
        entries <- readEntries caskpath
        pure $ mapEntriesToKeydir caskpath entries
        ) caskfiles
    pure $ Map.unionsWith compareByTimestamp keydirs

listKeysFromKeydir :: Keydir -> [Key]
listKeysFromKeydir = Map.keys
