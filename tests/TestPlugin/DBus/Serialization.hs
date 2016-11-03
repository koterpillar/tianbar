module TestPlugin.DBus.Serialization where

import Data.Aeson
import Data.Int
import qualified Data.Map as M
import Data.Maybe
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

limitedArbitrary :: Gen Variant
limitedArbitrary = oneof [ toVariant <$> (arbitrary :: Gen Bool)
                         , toVariant <$> (arbitrary :: Gen Int64)
                         , toVariant <$> (arbitrary :: Gen Double)
                         , toVariant <$> (T.pack <$> (arbitrary :: Gen String))
                         , toVariant <$> (arbitrary :: Gen ObjectPath)
                         , (toVariant . map lvVariant) <$> (half $ arbitrary :: Gen [LimitedVariant])
                         , (toVariant . M.map lvVariant) <$> (half $ arbitrary :: Gen (M.Map String LimitedVariant))]
    where half = scale (`div` 2)

instance Arbitrary ObjectPath where
    arbitrary = (fromJust . parseObjectPath  . concatPath) <$> listOf1 arbitraryAlpha
        where concatPath :: [String] -> String
              concatPath = concat . map ('/':)
              arbitraryAlpha :: Gen String
              arbitraryAlpha = listOf1 $ elements ['a'..'z']

jsonRoundTrip :: TestTree
jsonRoundTrip = QC.testProperty "Variant's FromJSON and ToJSON are inverse of each other" $
    \(LimitedVariant val) -> decode (encode val) == Just val
