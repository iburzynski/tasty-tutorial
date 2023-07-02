module ParseIPSpec where

import Data.Maybe (catMaybes)
import IPTypes (IP (..), IPRange (..), IPRangeDB (..), ParseError)
import ParseIP (buildIP, parseIP, parseIPRange, parseIPRanges)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Printf (printf)

parseIPSpecs :: Spec
parseIPSpecs = describe "ParseIP" $ do
  spec_buildIP
  spec_parseIP
  spec_parseIPRange
  spec_parseIPRanges

spec_buildIP :: Spec
spec_buildIP = describe "buildIP" $ do
  it "builds from zero" $
    buildIP [0, 0, 0, 0] `shouldBe` IP 0
  it "builds from one" $
    buildIP [0, 0, 0, 1] `shouldBe` IP 1
  it "builds from localhost" $
    buildIP [127, 0, 0, 1] `shouldBe` IP (1 + 127 * 256 ^ 3)
  it "builds from arbitrary address" $
    buildIP [192, 168, 3, 15] `shouldBe` IP (15 + 3 * 256 + 168 * 256 ^ 2 + 192 * 256 ^ 3)

spec_parseIP :: Spec
spec_parseIP =
  describe "parseIP" $ do
    it "parses zero" $
      parseIP "0.0.0.0" `shouldBe` justIP [0]
    it "parses one" $
      parseIP "0.0.0.1" `shouldBe` justIP [1]
    it "parses the largest IP address" $
      parseIP "255.255.255.255" `shouldBe` Just (IP maxBound)
    it "parses some random IP address" $
      parseIP "192.168.3.15"
        `shouldBe` justIP [192, 168, 3, 15]
    it "fails to parse 3 components" $
      parseIP "192.168.1" `shouldBe` Nothing
    it "fails to parse 4 components with suffix" $
      parseIP "192.168.1.1x" `shouldBe` Nothing
    it "fails to parse empty component" $
      parseIP "192.168..1" `shouldBe` Nothing
    it "fails to parse empty component in the end" $
      parseIP "192.168.0." `shouldBe` Nothing
    it "fails to parse empty component in the beginning" $
      parseIP ".192.168.0" `shouldBe` Nothing
    it "fails to parse 5 components" $
      parseIP "192.168.1.0.1" `shouldBe` Nothing
    it "fails to parse large components" $
      parseIP "256.168.1.0" `shouldBe` Nothing
    it "fails to parse extremely large components" $
      parseIP "0.0.0.4294967338" `shouldBe` Nothing
    it "fails to parse even larger components (2^65)" $
      parseIP "0.0.0.36893488147419103232" `shouldBe` Nothing
    it "fails to parse negative components" $
      parseIP "256.168.-1.0" `shouldBe` Nothing
    it "fails to parse non-numeric components" $
      parseIP "192.x.1.0" `shouldBe` Nothing
  where
    justIP = Just . buildIP

spec_parseIPRange :: Spec
spec_parseIPRange =
  describe "parseIPRange" $ do
    it "parses pair" $
      parseIPRange "192.168.0.1,192.168.3.100" `shouldBe` do
        ip1 <- parseIP "192.168.0.1"
        ip2 <- parseIP "192.168.3.100"
        Just $ IPRange ip1 ip2
    it "fails to parse single IP" $
      parseIPRange "192.168.0.1" `shouldBe` Nothing
    it "fails to parse single IP with comma" $
      parseIPRange "192.168.0.1," `shouldBe` Nothing
    it "fails to parse triple" $
      parseIPRange "192.168.0.1,192.168.0.2,192.168.0.4" `shouldBe` Nothing

spec_parseIPRanges :: Spec
spec_parseIPRanges =
  describe "parseIPRanges" $ do
    it "parses empty list" $
      parseIPRanges "" `shouldBe` Right mempty
    it "parses single range" $
      parseIPRanges sample_range1
        `shouldBe` rDBfromParsed [parseIPRange sample_range1]
    it "parses two ranges" $
      parseIPRanges sample_ranges
        `shouldBe` rDBfromParsed
          [ parseIPRange sample_range1,
            parseIPRange sample_range2
          ]
  where
    sample_range1 = "192.168.0.1,192.168.3.100"
    sample_range2 = "0.0.0.0,0.0.0.1"
    sample_ranges = printf "%s\n%s" sample_range1 sample_range2
    rDBfromParsed :: [Maybe IPRange] -> Either ParseError IPRangeDB
    rDBfromParsed = Right . IPRangeDB . catMaybes