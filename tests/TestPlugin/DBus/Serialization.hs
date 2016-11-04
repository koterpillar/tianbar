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
import System.Tianbar.Plugin.DBus.Utils (unwrapVariants)


tests :: TestTree
tests = testGroup "Serialization" [jsonRoundTrip]


-- JSON round-trip identity doesn't make sense for all different Variant types,
-- such as different size integers. Limit the Arbitrary instance to the only
-- ones we care about
newtype WrapVariant = WrapVariant { wrapV :: Variant }
    deriving (Eq)

instance Show WrapVariant where
    show = show . wrapV

instance Arbitrary WrapVariant where
    arbitrary = WrapVariant <$> limitedArbitrary

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
simpleVariant TypeObjectPath =  (toVariant . unwrapOP) <$> (arbitrary :: Gen WrapObjectPath)
simpleVariant TypeVariant = (toVariant . wrapV) <$> (half $ arbitrary :: Gen WrapVariant)
simpleVariant typ = error $ show typ ++ " isn't a simple Variant type."

simpleVariantArray :: Type -> Gen Variant
simpleVariantArray TypeBoolean = unwrapVariants (Proxy :: Proxy Bool) <$> listOf1 (simpleVariant TypeBoolean)
simpleVariantArray TypeInt64 = unwrapVariants (Proxy :: Proxy Int64) <$> listOf1 (simpleVariant TypeInt64)
simpleVariantArray TypeDouble = unwrapVariants (Proxy :: Proxy Double) <$> listOf1 (simpleVariant TypeDouble)
simpleVariantArray TypeString = unwrapVariants (Proxy :: Proxy T.Text) <$> listOf1 (simpleVariant TypeString)
simpleVariantArray TypeObjectPath = unwrapVariants (Proxy :: Proxy ObjectPath) <$> listOf1 (simpleVariant TypeObjectPath)
simpleVariantArray TypeVariant = unwrapVariants (Proxy :: Proxy Variant) <$> listOf1 (simpleVariant TypeVariant)
simpleVariantArray typ = error $ show typ ++ " isn't a simple Variant type."

simpleVariantDictionary :: Type -> Gen Variant
simpleVariantDictionary TypeBoolean = unwrapVariants (Proxy :: Proxy Bool) <$> stringMapOf1 (simpleVariant TypeBoolean)
simpleVariantDictionary TypeInt64 = unwrapVariants (Proxy :: Proxy Int64) <$> stringMapOf1 (simpleVariant TypeInt64)
simpleVariantDictionary TypeDouble = unwrapVariants (Proxy :: Proxy Double) <$> stringMapOf1 (simpleVariant TypeDouble)
simpleVariantDictionary TypeString = unwrapVariants (Proxy :: Proxy T.Text) <$> stringMapOf1 (simpleVariant TypeString)
simpleVariantDictionary TypeObjectPath = unwrapVariants (Proxy :: Proxy ObjectPath) <$> stringMapOf1 (simpleVariant TypeObjectPath)
simpleVariantDictionary TypeVariant = unwrapVariants (Proxy :: Proxy Variant) <$> stringMapOf1 (simpleVariant TypeVariant)
simpleVariantDictionary typ = error $ show typ ++ " isn't a simple Variant type."

stringMapOf1 :: Gen a -> Gen (M.Map String a)
stringMapOf1 gen = do
    values <- listOf1 gen
    keys <- vector (length values)
    return $ M.fromList (zip keys values)

limitedArbitrary :: Gen Variant
limitedArbitrary = oneof $ map (oneof . flip map simpleTypes) [ simpleVariant
                                                              , simpleVariantArray
                                                              , simpleVariantDictionary
                                                              ]

newtype WrapObjectPath = WrapObjectPath { unwrapOP :: ObjectPath }
    deriving (Eq)

instance Show WrapObjectPath where
    show = show . unwrapOP

instance Arbitrary WrapObjectPath where
    arbitrary = (WrapObjectPath . fromJust . parseObjectPath  . concatPath) <$> listOf1 arbitraryAlpha
        where concatPath :: [String] -> String
              concatPath = concat . map ('/':)
              arbitraryAlpha :: Gen String
              arbitraryAlpha = listOf1 $ elements ['a'..'z']

jsonRoundTrip :: TestTree
jsonRoundTrip = QC.testProperty "Variant's FromJSON and ToJSON are inverse of each other" $
    \(WrapVariant val) -> decode (encode val) == Just val
