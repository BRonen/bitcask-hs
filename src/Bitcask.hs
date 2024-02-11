module Bitcask (open, put, get, delete, listKeys, merge, close) where

import qualified Data.ByteString.Lazy as B

import Data.String.Interpolate (i)
import System.FileLock (FileLock, SharedExclusive (..), tryLockFile, unlockFile)
import System.FilePath ((</>), dropFileName)

import Entry (Entry (..), Key, Value, buildEntry, nanosSinceEpoch)
import Keydir (buildKeyDir, getValueFromKeydir, listKeysFromKeydir)
import Caskfile (getCurrentFileId, prependEntry, removePrevFiles)

data Handle = Handle FilePath Bool FileLock

instance Show Handle where
    show (Handle filepath isWriter _) = [i|Handle - #{filepath} #{isWriter} lockfile|]
    
tombstone :: B.ByteString
tombstone = "__BITCASK_TOMBSTONE__"

open :: String -> Bool -> IO (Either String Handle)
open dirpath isWriter = do
    currentFileid <- getCurrentFileId dirpath
    let filepath = dirpath </> show currentFileid ++ ".cask"
    let lockpath = dirpath </> "cask.lock"
    let locktype = if isWriter then Exclusive else Shared
    flock <- tryLockFile lockpath locktype
    pure $ case flock of
        Just flock' -> Right $ Handle filepath isWriter flock'
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
    keydir <- buildKeyDir (dropFileName filepath)
    value <- getValueFromKeydir keydir key
    pure $ case value of
        Just value' -> if value' == tombstone then Nothing else Just value'
        Nothing -> Nothing

delete :: Handle -> Key -> IO (Either String ())
delete handle key = do
    e <- put handle key tombstone
    pure $ case e of
        Left err -> Left err
        _ -> Right ()

listKeys :: Handle -> IO [Key]
listKeys (Handle filepath _ _) = do
    keydir <- buildKeyDir (dropFileName filepath)
    pure $ listKeysFromKeydir keydir

merge :: Handle -> IO (Either String Handle)
merge (Handle filepath' True filelock) = do
    let dirpath = dropFileName filepath'
    currentFileid <- getCurrentFileId dirpath
    let filepath = dirpath </> show currentFileid ++ ".cask"
    let handle' = Handle filepath True filelock
    keydir <- buildKeyDir (dropFileName filepath)
    let keys = listKeysFromKeydir keydir
    mapM_ (\key -> do
        value <- get handle' key
        case value of
            Just value' -> put handle' key value'
            Nothing -> pure $ Left "wasd"
        ) keys
    removePrevFiles filepath
    pure $ Right handle'
merge _ = do
    pure $ Left "Unable to merge on this instance"

{- sync :: Handle -> () -}

close :: Handle -> IO ()
close (Handle _ _ filelock) = unlockFile filelock
