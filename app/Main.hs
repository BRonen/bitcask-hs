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
            _ <- BC.delete handle "hello"
            case entry of
                Right entry' -> do
                    print entry'
                    value <- BC.get handle "hello"
                    print value
                Left err -> print err
            BC.listKeys handle >>= print
            BC.close handle
        Left err -> print err
