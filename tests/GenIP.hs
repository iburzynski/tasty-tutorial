{-# OPTIONS_GHC -Wno-unused-imports #-}

module GenIP where

import Control.Monad (replicateM_)
import Data.List (intercalate)
import Data.Word (Word32, Word8)
import Hedgehog (Gen, MonadGen)
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R
import IPTypes (IP (..), IPRange (..), IPRangeDB (..))

-- type    Gen      = GenT Identity
-- newtype GenT m a = GenT { unGenT :: Size -> Seed -> TreeT (MaybeT m) a }
-- TreeT: an effectful tree - nodes can have an effect before they're produced
-- MonadGen typeclass specifies how to convert a monadic context into GenT, allowing more complex transformer stacks

-- | A generator for a single IP, using a linear range between Word32 bounds
genIP :: Gen IP
genIP = IP <$> G.word32 R.linearBounded

-- | A generator for the Word8 components of an IP
genIPComponents :: Gen [Word8]
genIPComponents = G.list (R.singleton 4) genOctet
  where
    genOctet = G.word8 R.linearBounded

-- | A generator for a single IP string
genIPString :: Gen String
genIPString = intercalate "." . map show <$> genIPComponents

-- | Helper function to construct an IPRange value in a MonadGen context for two Word32s
mkGenIPRange :: MonadGen m => Word32 -> Word32 -> m IPRange
mkGenIPRange ip1 ip2 = pure $ IPRange (IP ip1) (IP ip2)

-- | Generate an IP
genIPRange :: Gen IPRange
genIPRange = do
  (IP ip1) <- genIP
  ip2 <- G.word32 (R.linearFrom (ip1 + 1) ip1 maxBound)
  mkGenIPRange ip1 ip2

-- | Generate an IP range with first IP higher than second
genInvalidIPRange :: Gen IPRange
genInvalidIPRange = do
  -- use filter to prevent first IP being 0 (since all such ranges are valid)
  (IP ip1) <- G.filter (> IP minBound) genIP
  -- make second IP lower than first
  ip2 <- G.word32 (R.linear minBound (ip1 - 1))
  mkGenIPRange ip1 ip2

-- | Generates a random database containing 1-100 IP ranges
genIPRangeDB :: Gen IPRangeDB
genIPRangeDB = do
  -- `integral` generator can generate a value of any `Integral` type (here `Int`)
  n1 <- G.integral $ R.constant 1 100 -- Get a random Int between 1 and 100
  n2 <- G.integral $ R.constant n1 100 -- Get a random Int between n1 and 100
  -- Get a random list of n1 to 100 IP ranges
  genIPRangeDBSized n1 n2

-- | Generates random lists of IP ranges with length between specified bounds
genIPRangeDBSized :: Int -> Int -> Gen IPRangeDB
genIPRangeDBSized minLen maxLen = IPRangeDB <$> G.list (R.constant minLen maxLen) genIPRange

-- | Util that outputs a sample of random values to a file
writeFileWithRanges :: Int -> FilePath -> IO ()
writeFileWithRanges n fp = do
  str <- show <$> G.sample (genIPRangeDBSized n n)
  writeFile fp str