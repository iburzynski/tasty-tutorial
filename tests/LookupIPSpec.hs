module LookupIPSpec where

import IPTypes (IP (..), IPRange (..), IPRangeDB (..))
import LookupIP (lookupIP)
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldNotSatisfy,
    shouldSatisfy,
  )
import Data.Word (Word32)

-- type    Spec         = SpecWith ()
-- type    SpecWith a   = SpecM a ()
-- newtype SpecM a r    = SpecM { unSpecM :: WriterT (Endo Config, [SpecTree  a]) (ReaderT Env IO) r  }
--         Spec         = SpecM { unSpecM :: WriterT (Endo Config, [SpecTree ()]) (ReaderT Env IO) () }

lookupIPSpecs :: Spec
lookupIPSpecs = describe "LookupIP" spec_lookupIP

spec_lookupIP :: Spec
spec_lookupIP =
  describe "lookupIP" $ do
    it "no IP in empty list" $
      ip1 `shouldNotSatisfy` lookupIP mempty
    it "IP in sample list" $
      ip1 `shouldSatisfy` lookupIP sample_iprdb
    it "no IP in sample list" $
      ip2 `shouldNotSatisfy` lookupIP sample_iprdb
  where
    mkIPRange :: Word32 -> Word32 -> IPRange
    mkIPRange i1 i2 = IPRange (IP i1) (IP i2)
    sample_iprdb = IPRangeDB [mkIPRange 0 1, mkIPRange 100 200]
    ip1 = IP 110
    ip2 = IP 50