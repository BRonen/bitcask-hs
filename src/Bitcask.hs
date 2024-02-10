module Bitcask where

import qualified Data.ByteString.Lazy.UTF8 as BU

import System.FileLock (FileLock, SharedExclusive (..), tryLockFile, unlockFile)
import System.FilePath (takeDirectory)

import Entry (Entry (..), Key, Value, buildEntry, nanosSinceEpoch)
import Keydir (buildKeyDir, getValueFromKeydir)
import Caskfile (getCurrentFileId, prependEntry)

data Handle = Handle FilePath Bool FileLock

open :: String -> Bool -> IO (Either String Handle)
open dirpath isWriter = do
    currentFileid <- getCurrentFileId dirpath
    let filepath = dirpath ++ show currentFileid ++ ".cask"
    let locktype = if isWriter then Exclusive else Shared
    flock <- tryLockFile filepath locktype
    pure $ case flock of
        Just flock -> Right $ Handle filepath isWriter flock
        Nothing -> Left "Unable to lock file"

put :: Handle -> Key -> Value -> IO (Either String Entry)
put (Handle filepath isWriter _) key value = do
    if isWriter
        then do
            timestamp <- nanosSinceEpoch
            let entry = buildEntry timestamp key value
            entry' <- prependEntry filepath entry
            pure $ Right entry'
        else pure $ Left "Not a writer instance"

get :: Handle -> Key -> IO (Maybe String)
get (Handle filepath _ _) key = do
    keydir <- buildKeyDir (takeDirectory filepath)
    value <- getValueFromKeydir keydir key
    print value
    pure $ case value of
        Just value -> Just $ BU.toString value
        Nothing -> Nothing

{-
delete :: Handle -> Key -> Maybe Entry

listKeys :: Handle -> [Key]

merge :: Handle -> ()

sync :: Handle -> () -}

close :: Handle -> IO ()
close (Handle _ _ filelock) = unlockFile filelock
