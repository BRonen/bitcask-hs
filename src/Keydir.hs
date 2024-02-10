module Keydir where

import qualified Data.Map as Map

import System.FilePath ((</>))
import Caskfile (listCaskFiles, readEntries, getFileIdFromPath, readValueFromPos)
import Entry (Entry (..), Checksum, FieldSize, Timestamp, Key, Value)

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
            value <- readValueFromPos filepath vsize $ fromIntegral offset
            pure $ Just value
        Nothing -> pure Nothing

buildKeyDir :: FilePath -> IO Keydir
buildKeyDir dirpath = do
    caskfiles <- listCaskFiles dirpath
    keydirs <- mapM (\caskfile -> do
        let caskpath = dirpath ++ "/" ++ caskfile
        entries <- readEntries caskpath
        print entries
        pure $ mapEntriesToKeydir caskpath entries
        ) caskfiles
    pure $ Map.unions keydirs

listKeysFromKeydir :: Keydir -> [Key]
listKeysFromKeydir = Map.keys
