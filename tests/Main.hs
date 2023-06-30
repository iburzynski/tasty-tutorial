module Main (main) where

import LookupIPSpec (lookupIPSpecs)
import ParseIPSpec (parseIPSpecs)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpecs)

main :: IO ()
main = do
  specs <- concat <$> traverse testSpecs [lookupIPSpecs, parseIPSpecs]
  defaultMain $
    testGroup
      "All Tests"
      [testGroup "Specs" specs]