module TestPlugin.DBus.Serialization where

import Data.Aeson
import Data.Int
import qualified Data.Map as M
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T

import DBus

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import System.Tianbar.Plugin.DBus.FromData ()
import System.Tianbar.Plugin.DBus.JSON ()


tests :: TestTree
tests = testGroup "Serialization" [jsonRoundTrip]


-- JSON round-trip identity doesn't make sense for all different Variant types,
-- such as different size integers. Limit the Arbitrary instance to the only
-- ones we care about
newtype LimitedVariant = LimitedVariant { lvVariant :: Variant }
    deriving (Eq)

instance Show LimitedVariant where
    show = show . lvVariant

instance Arbitrary LimitedVariant where
    arbitrary = LimitedVariant <$> limitedArbitrary

half :: Gen a -> Gen a
half = scale (`div` 2)

simpleTypes :: [Type]
simpleTypes = [ TypeBoolean
              , TypeInt64
              , TypeDouble
              , TypeString
              , TypeObjectPath
              , TypeVariant
              ]

simpleVariant :: Type -> Gen Variant
simpleVariant TypeBoolean = toVariant <$> (arbitrary :: Gen Bool)
simpleVariant TypeInt64 = toVariant <$> (arbitrary :: Gen Int64)
simpleVariant TypeDouble = toVariant <$> (arbitrary :: Gen Double)
simpleVariant TypeString = (toVariant . T.pack) <$> (arbitrary :: Gen String)
simpleVariant TypeObjectPath =  toVariant <$> (arbitrary :: Gen ObjectPath)
simpleVariant TypeVariant = (toVariant . lvVariant) <$> (half $ arbitrary :: Gen LimitedVariant)
simpleVariant typ = error $ show typ ++ " isn't a simple Variant type."

unwrapVariantArray :: IsValue v => Proxy v -> [Variant] -> Variant
unwrapVariantArray typ = toVariant . map ((`asProxyTypeOf` typ) . fromJust . fromVariant)

simpleVariantArray :: Type -> Gen Variant
simpleVariantArray TypeBoolean = unwrapVariantArray (Proxy :: Proxy Bool) <$> listOf1 (simpleVariant TypeBoolean)
simpleVariantArray TypeInt64 = unwrapVariantArray (Proxy :: Proxy Int64) <$> listOf1 (simpleVariant TypeInt64)
simpleVariantArray TypeDouble = unwrapVariantArray (Proxy :: Proxy Double) <$> listOf1 (simpleVariant TypeDouble)
simpleVariantArray TypeString = unwrapVariantArray (Proxy :: Proxy T.Text) <$> listOf1 (simpleVariant TypeString)
simpleVariantArray TypeObjectPath = unwrapVariantArray (Proxy :: Proxy ObjectPath) <$> listOf1 (simpleVariant TypeObjectPath)
simpleVariantArray TypeVariant = unwrapVariantArray (Proxy :: Proxy Variant) <$> listOf1 (simpleVariant TypeVariant)
simpleVariantArray typ = error $ show typ ++ " isn't a simple Variant type."

-- TODO: Generate dictionaries of uniform types

limitedArbitrary :: Gen Variant
limitedArbitrary = oneof $ map simpleVariant simpleTypes ++ map simpleVariantArray simpleTypes ++ complexGen
    where complexGen = [ (toVariant . M.map lvVariant) <$> (half $ arbitrary :: Gen (M.Map String LimitedVariant))]

instance Arbitrary ObjectPath where
    arbitrary = (fromJust . parseObjectPath  . concatPath) <$> listOf1 arbitraryAlpha
        where concatPath :: [String] -> String
              concatPath = concat . map ('/':)
              arbitraryAlpha :: Gen String
              arbitraryAlpha = listOf1 $ elements ['a'..'z']

jsonRoundTrip :: TestTree
jsonRoundTrip = QC.testProperty "Variant's FromJSON and ToJSON are inverse of each other" $
    \(LimitedVariant val) -> decode (encode val) == Just val
