{-# LANGUAGE TypeApplications #-}

module ParseIP where

import Control.Monad (guard)
import Data.Bits (toIntegralSized)
import Data.Foldable (traverse_)
import Data.List.Split (splitOn)
import Data.Word (Word32, Word8)
import IPTypes (IP (..), IPRange (..), IPRangeDB (..), LineNumber, ParseError (..))
import Text.Read (readMaybe)

-- Convert a list of IP address components into an IP
buildIP :: [Word8] -> IP
buildIP components = IP . fst $ foldr combineComponents (0, 1) components
  where
    combineComponents :: Word8 -> (Word32, Word32) -> (Word32, Word32)
    combineComponents component (result, multiplier) =
      (result + fromIntegral component * multiplier, multiplier * 256)

-- Parse an IP address string to an IP if valid
parseIP :: String -> Maybe IP
parseIP ipString = do
  let componentStrs = splitOn "." ipString
  guard (length componentStrs == 4) -- Return Nothing if wrong number of components
  components <- traverse readMaybe componentStrs -- Parse component strings to Word8 components
  traverse_ (guard . fitsOctet) components -- Return Nothing if any component invalid
  Just (buildIP components)
  where
    -- Check if component is within valid numeric range (0 - 255)
    fitsOctet :: Word8 -> Bool
    fitsOctet component = 0 <= component && component <= 255

parseIPRange :: String -> Maybe IPRange
parseIPRange rangeStr = do
  let rangeStrs = splitOn "," rangeStr
  ips <- traverse parseIP rangeStrs -- Parse strings to IPs (returns Nothing if any component invalid)
  case ips of
    [l, u] -> Just (IPRange l u) -- Construct IPRange if exactly two IPs
    _ -> Nothing

parseIPRanges :: String -> Either ParseError IPRangeDB
parseIPRanges inputStr =
  fmap IPRangeDB . traverse parseLine . zip [1 ..] $ lines inputStr
  where
    parseLine :: (LineNumber, String) -> Either ParseError IPRange
    parseLine (ln, str) = case parseIPRange str of
      Nothing -> Left (ParseError ln)
      Just ipRange -> Right ipRange

--------

-- Alternative implementation of `parseIP` that satisfies failing tests
parseIP' :: String -> Maybe IP
parseIP' ipString = do
  let componentStrs = splitOn "." ipString
  guard (length componentStrs == 4) -- Return Nothing if wrong number of components
  components <- traverse (\c -> readMaybe @Integer c >>= toIntegralSized) componentStrs -- Parse component strings to Word8 components
  Just (buildIP components)