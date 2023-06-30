{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}

module IPTypes where

import Control.Monad.Catch (Exception)
import Data.List (intercalate)
import Data.Word (Word32)
import Text.Printf (printf)

newtype IP = IP {unIP :: Word32}
  deriving (Eq, Ord)

instance Show IP where
  show (IP ip) = intercalate "." $ unfoldIP ip []
    where
      unfoldIP :: Word32 -> [String] -> [String]
      unfoldIP 0 !componentStrs = componentStrs
      unfoldIP n !componentStrs = unfoldIP (n `div` 256) (show (n `mod` 256) : componentStrs)

data IPRange = IPRange IP IP
  deriving (Eq)

instance Show IPRange where
  show (IPRange ip1 ip2) = printf "%s,%s" (show ip1) (show ip2)

newtype IPRangeDB = IPRangeDB [IPRange]
  deriving (Eq)

instance Show IPRangeDB where
  show (IPRangeDB ipRanges) = unlines $ map show ipRanges

type LineNumber = Int

newtype ParseError = ParseError LineNumber
  deriving (Show, Eq)

data InvalidArgsException = LoadIPRangesError ParseError | InvalidIP String
  deriving (Exception) -- Requires {-# LANGUAGE DeriveAnyClass #-}

instance Show InvalidArgsException where
  show (LoadIPRangesError (ParseError idx)) =
    printf "Error loading IP range databases (line: %s)" $ show idx
  show (InvalidIP s) = "Invalid IP address to check: " ++ s