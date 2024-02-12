module Unit.BitcaskSpec (test) where

import qualified Bitcask as BC
import Control.Exception (bracket)
import System.Directory (createDirectory, removeDirectoryRecursive)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

withTemporaryDirectory :: FilePath -> (FilePath -> Assertion) -> Assertion
withTemporaryDirectory tmpdir = bracket setUp tearDown
  where
    setUp = createDirectory tmpdir >> pure tmpdir
    tearDown _ = removeDirectoryRecursive tmpdir

test :: TestTree
test = testGroup "Bitcask module" [testClose, testOpen]

testOpen :: TestTree
testOpen = testGroup "Open function" [test1]

testClose :: TestTree
testClose = testGroup "Close function" [test2]

test1 :: TestTree
test1 = testCase "Should error on attempt to instance a second writer" $
  withTemporaryDirectory "temp1" $ \tmpDir -> do
    firsthandle <- BC.open tmpDir True
    case firsthandle of
      Left _ -> assertFailure "Unable to open one bitcask handle"
      Right _ -> do
        secondhandle <- BC.open tmpDir True
        case secondhandle of
          Right _ -> assertFailure "Able to instance two writers at same time"
          Left err -> err @?= "Unable to lock file"

test2 :: TestTree
test2 = testCase "Should instance a second writer after close first" $
  withTemporaryDirectory "temp2" $ \tmpDir -> do
    firsthandle <- BC.open tmpDir True
    case firsthandle of
      Left _ -> assertFailure "Unable to open one bitcask handle"
      Right firsthandle' -> do
        BC.close firsthandle'
        secondhandle <- BC.open tmpDir True
        case secondhandle of
          Right _ -> pure ()
          Left err -> assertFailure err
