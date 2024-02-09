module Keydir where

import qualified Data.Map as Map

import Caskfile (listCaskFiles, readEntries)
import Entry (Entry (..), Checksum, FieldSize, Timestamp, Key)

data KeydirEntry = KeydirEntry Checksum FieldSize Int Timestamp
    deriving (Show, Eq)

type Keydir = Map.Map Key KeydirEntry

mapEntriesToKeydir :: [(Int, Entry)] -> Keydir
mapEntriesToKeydir [] = Map.empty
mapEntriesToKeydir ((offset, Entry _ timestamp _ vsize key _):entries) =
    let entry = KeydirEntry 18 vsize offset timestamp in
    Map.insert key entry $ mapEntriesToKeydir entries

buildKeyDir :: IO Keydir
buildKeyDir = do
    caskfiles <- listCaskFiles
    keydirs <- mapM (\caskfile -> do
        entries <- readEntries caskfile
        print entries
        pure $ mapEntriesToKeydir entries
        ) caskfiles
    pure $ Map.unions keydirs
