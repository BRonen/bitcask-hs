module Main (main) where

{- import Entry
import Caskfile
import Keydir -}

import qualified Bitcask as BC

main :: IO ()
main = do
    handle' <- BC.open "temp/" True
    case handle' of
        Right handle -> do
            print "success"
            BC.put handle "hello" "world"
            entry <- BC.put handle "hello1" "world2"
            case entry of
                Right entry' -> do
                    print entry'
                    value <- BC.get handle "hello1"
                    print value
                Left err -> print err
            BC.close handle
        Left err -> print err

    {- currentFileid <- getCurrentFileId
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
    pure () -}
