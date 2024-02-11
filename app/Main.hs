module Main (main) where

{- import Entry
import Caskfile
import Keydir -}

import qualified Bitcask as BC

main :: IO ()
main = do
    handle' <- BC.open "temp/" True
    print handle'
    case handle' of
        Left err -> print err
        Right handle -> do
            {- BC.put handle "hello3" "world3"
            _ <- BC.delete handle "hello"
            case entry of
                Right entry' -> do
                    print entry'
                Left err -> print err -}
            BC.get handle "hello" >>= print
            BC.put handle "hello" "999999" >>= print
            BC.listKeys handle >>= print
            handle'' <- BC.merge handle
            case handle'' of
                Left err -> print err
                Right handle''' -> do
                    BC.listKeys handle''' >>= print
                    BC.put handle''' "joao castro" "grande"
                    BC.put handle''' "joao castro1" "grande2"
                    BC.close handle'''
            pure ()
