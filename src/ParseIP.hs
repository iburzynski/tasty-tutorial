{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module ParseIP where

import Control.Monad (guard)
import Data.Bits (shiftL, toIntegralSized)
import Data.Foldable (foldl', traverse_)
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

-- "192.168.0.1,192.168.3.100"
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
  components <- traverse convertComponent componentStrs -- Parse component strings to Word8 components
  Just (buildIP components)
  where
    -- read to an unbounded integer to prevent overflow, then try to convert to Word8
    convertComponent :: String -> Maybe Word8
    convertComponent c = do
      i <- readMaybe @Integer c
      toIntegralSized i

-- Alternative implementation of `parseIPRange` that satisfies failing test
parseIPRange' :: String -> Maybe IPRange
parseIPRange' rangeStr = do
  let rangeStrs = splitOn "," rangeStr
  ips <- traverse parseIP rangeStrs
  case ips of
    -- make sure range isn't invalid
    [l, u] | l <= u -> Just (IPRange l u)
    _ -> Nothing

-----

-- Alternative implementations of `buildIP`:
buildIP_foldl :: (Word32 -> Word8 -> Word32) -> [Word8] -> IP
buildIP_foldl reducer components = IP $ foldl' reducer 0 components

buildIP_foldl_reg :: [Word8] -> IP
buildIP_foldl_reg = buildIP_foldl combineComponents
  where
    combineComponents :: Word32 -> Word8 -> Word32
    combineComponents result component = result * 256 + fromIntegral component

buildIP_foldl_bw :: [Word8] -> IP
buildIP_foldl_bw = buildIP_foldl combineComponents
  where
    combineComponents :: Word32 -> Word8 -> Word32
    combineComponents result component = shiftL result 8 + fromIntegral component