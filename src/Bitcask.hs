module Bitcask where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BU

import System.FileLock (FileLock, SharedExclusive (..), tryLockFile, unlockFile)
import System.FilePath (takeDirectory)

import Entry (Entry (..), Key, Value, buildEntry, nanosSinceEpoch)
import Keydir (buildKeyDir, getValueFromKeydir, listKeysFromKeydir)
import Caskfile (getCurrentFileId, prependEntry)

data Handle = Handle FilePath Bool FileLock

tombstone :: B.ByteString
tombstone = "__BITCASK_TOMBSTONE__"

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

get :: Handle -> Key -> IO (Maybe Value)
get (Handle filepath _ _) key = do
    keydir <- buildKeyDir (takeDirectory filepath)
    value <- getValueFromKeydir keydir key
    pure $ case value of
        Just value -> if value == tombstone then Nothing else Just value
        Nothing -> Nothing

delete :: Handle -> Key -> IO (Either String ())
delete handle key = do
    e <- put handle key tombstone
    pure $ case e of
        Left err -> Left err
        _ -> Right ()

listKeys :: Handle -> IO [Key]
listKeys (Handle filepath _ _) = do
    keydir <- buildKeyDir (takeDirectory filepath)
    pure $ listKeysFromKeydir keydir

merge :: Handle -> IO (Either String Handle)
merge (Handle filepath _ filelock) = do
    unlock filelock
    handle <- open (takeDirectory filepath) true
    case handle of
        Right handle' -> do
            keydir <- buildKeyDir (takeDirectory filepath)
            let keys = listKeysFromKeydir keydir

        _ -> handle

{- sync :: Handle -> () -}

close :: Handle -> IO ()
close (Handle _ _ filelock) = unlockFile filelock
