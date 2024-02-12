module Unit.BitcaskSpec (test) where

import Test.Tasty
import Test.Tasty.HUnit

test :: TestTree
test = testGroup "Bitcask" [test2]

test2 :: TestTree
test2 = testCase "7 is even" $ assertBool "Oops, 7 is odd" (odd (7 :: Integer))