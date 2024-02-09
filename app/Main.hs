module Main (main) where

import qualified Data.ByteString.Lazy.UTF8 as BU

import Entry
import Caskfile
import Keydir

main :: IO ()
main = do
    currentFileid <- getCurrentFileId
    t <- nanosSinceEpoch
    _ <- prependEntry currentFileid $ buildEntry t (BU.fromString "hello12") (BU.fromString "world1")
    _ <- prependEntry currentFileid $ buildEntry t (BU.fromString "hello2") (BU.fromString "world24")
    a <- prependEntry currentFileid $ buildEntry t (BU.fromString "hello33") (BU.fromString "world3")
    print a
    entries <- readEntries $ show currentFileid ++ ".cask"
    _ <- mapM print entries
    print $ mapEntriesToKeydir entries
    
    keydir <- buildKeyDir
    print keydir
    pure ()
