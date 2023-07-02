{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Props where

import GenIP (genIP, genIPComponents, genIPRange, genIPRangeDB, genInvalidIPRange)
import Hedgehog (Property, assert, forAll, property, tripping, (===))
import Hedgehog.Gen (element)
import IPTypes (IPRange (..), IPRangeDB (..))
import LookupIP (lookupIP)
import ParseIP (buildIP, buildIP_foldl_bw, buildIP_foldl_reg, parseIP, parseIPRange, parseIPRanges)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

-- | Different `buildIP` implementations should agree with each other
prop_buildIPs :: Property
prop_buildIPs = property $ do
  ipcs <- forAll genIPComponents
  let ip = buildIP ipcs
  buildIP_foldl_reg ipcs === ip
  buildIP_foldl_bw ipcs === ip

prop_parseIP :: Property
prop_parseIP = property $ do
  ip <- forAll genIP
  parseIP (show ip) === Just ip

prop_parseIP_show :: Property
prop_parseIP_show = property $ do
  ip <- forAll genIP
  tripping ip show parseIP

-- `tripping` tests that a pair of encode/decode functions are compatible by making a "roundtrip".
-- Given a printer from some type `a -> b`, and a parser with a potential failure case `b -> f a`:
-- Ensure that a valid `a` round trips through the "print" and "parse" to yield the same `a`

prop_parseIPRange_show :: Property
prop_parseIPRange_show = property $ do
  ipr <- forAll genIPRange
  tripping ipr show parseIPRange

prop_parseIPRanges_show :: Property
prop_parseIPRanges_show = property $ do
  iprdb <- forAll genIPRangeDB
  tripping iprdb show parseIPRanges

prop_no_parseInvalidIPRange :: Property
prop_no_parseInvalidIPRange = property $ do
  inv_ip <- forAll genInvalidIPRange
  parseIPRange (show inv_ip) === Nothing

-- | No IP belongs to the empty IP range database
prop_lookupIP_empty :: Property
prop_lookupIP_empty = property $ do
  ip <- forAll genIP
  assert (not $ lookupIP mempty ip)

-- | Borders of every range in the database belong to the database
prop_lookupIP_bordersIncluded :: Property
prop_lookupIP_bordersIncluded = property $ do
  iprdb@(IPRangeDB iprdb') <- forAll genIPRangeDB
  IPRange ip1 ip2 <- forAll (element iprdb')
  let check = assert . lookupIP iprdb
  check ip1
  check ip2

-- | Assemble all properties as a list of Tasty TestTrees
props :: [TestTree]
props =
  [ testProperty "buildIP implementations agree with each other" prop_buildIPs,
    testProperty "parseIP works as expected" prop_parseIP,
    testProperty "parseIP agrees with show" prop_parseIP_show,
    testProperty "parseIPRange agrees with show" prop_parseIPRange_show,
    testProperty "parseIPRanges agrees with show" prop_parseIPRanges_show,
    testProperty "no parse of invalid IP ranges" prop_no_parseInvalidIPRange,
    testProperty "no IP in empty list" prop_lookupIP_empty,
    testProperty "lookupIP includes borders" prop_lookupIP_bordersIncluded
  ]