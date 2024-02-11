module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain $ testGroup "Root" [test1, test2]

test1 :: TestTree
test1 = testCase "2+2=4" $ (2 + 2 :: Integer) @?= (4 :: Integer)

test2 :: TestTree
test2 = testCase "7 is even" $ assertBool "Oops, 7 is odd" (odd (7 :: Integer))