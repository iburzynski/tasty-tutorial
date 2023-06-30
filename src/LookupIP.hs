module LookupIP where

import Data.List (find)
import IPTypes (IP (..), IPRange (..), IPRangeDB (..))
import Text.Printf (printf)

lookupIP :: IPRangeDB -> IP -> Bool
lookupIP (IPRangeDB ips) ip = case find inRange ips of
  Nothing -> False
  _ -> True
  where
    inRange :: IPRange -> Bool
    inRange (IPRange l u) = l <= ip && ip <= u

reportIP :: IPRangeDB -> IP -> String
reportIP iprDB ip = printf "%s: %s" (show ip) res
  where
    res = if lookupIP iprDB ip then "YES" else "NO"