{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Tianbar.Plugin.DBus.JSON () where

import Control.Applicative

import Data.Aeson hiding (Array)
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HM
import Data.Int
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word

import DBus

fromVariantJust :: IsVariant v => Variant -> v
fromVariantJust = fromJust . fromVariant

instance ToJSON Variant where
    toJSON v = case variantType v of
            TypeBoolean -> toJSON (fromVariantJust v :: Bool)
            TypeWord8 -> toJSON (fromVariantJust v :: Word8)
            TypeWord16 -> toJSON (fromVariantJust v :: Word16)
            TypeWord32 -> toJSON (fromVariantJust v :: Word32)
            TypeWord64 -> toJSON (fromVariantJust v :: Word64)
            TypeInt16 -> toJSON (fromVariantJust v :: Int16)
            TypeInt32 -> toJSON (fromVariantJust v :: Int32)
            TypeInt64 -> toJSON (fromVariantJust v :: Int64)
            TypeDouble -> doubleToJSON (fromVariantJust v :: Double)
            TypeString -> toJSON (fromVariantJust v :: T.Text)

            TypeSignature -> toJSON (fromVariantJust v :: Signature)
            TypeObjectPath -> toJSON (fromVariantJust v :: ObjectPath)

            TypeUnixFd -> toJSON (fromVariantJust v :: Word32)

            TypeVariant -> toJSON (fromVariantJust v :: Variant)
            TypeArray _ -> toJSON (arrayItems $ fromVariantJust v)
            TypeDictionary _ _ -> toJSON $ M.fromList $ map variantStringKey $
                dictionaryItems $ fromVariantJust v
            TypeStructure _ -> toJSON $ structureItems $ fromVariantJust v

-- Floating point zero is instantiated as "0". To distinguish from integer
-- zero, encode it as "0.0"
doubleToJSON :: Double -> A.Value
doubleToJSON 0 = A.Number (S.scientific 0 (-1))
doubleToJSON v = toJSON v

instance IsValue v => IsVariant (HM.HashMap T.Text v) where
    toVariant = toVariant . M.fromList . HM.toList
    fromVariant = fmap (HM.fromList . M.toList) . fromVariant

instance FromJSON Variant where
    parseJSON (Bool b) = pure $ toVariant b
    parseJSON (Number n) | S.base10Exponent n < 0 = pure $ toVariant doubleValue
                         | otherwise = pure $ toVariant (fromIntegral integerValue :: Int64)
        where doubleValue = S.toRealFloat n :: Double
              integerValue = S.coefficient n * (10 ^ S.base10Exponent n)
    parseJSON (String s) = pure $ toVariant s
    parseJSON (A.Array a) = toVariant <$> (mapM parseJSON (V.toList a) :: A.Parser [Variant])
    parseJSON v@(Object o) = parseObjectPathJSON v <|> parseMapJSON o
    parseJSON val = A.typeMismatch "Variant" val

parseObjectPathJSON :: Value -> A.Parser Variant
parseObjectPathJSON v = toVariant <$> (parseJSON v :: A.Parser ObjectPath)

parseMapJSON :: Object -> A.Parser Variant
parseMapJSON o = toVariant <$> (mapM parseJSON o :: A.Parser (HM.HashMap T.Text Variant))

variantString :: Variant -> String
variantString v = s
    where Just s = case variantType v of
                       TypeString -> fromVariant v
                       TypeVariant -> Just $ variantString v'
                           where v' = fromVariantJust v
                       _ -> Just $ show v

variantStringKey :: (Variant, Variant) -> (String, Variant)
variantStringKey (k, v) = (variantString k, v)

parseStringMarker :: T.Text -> (String -> Maybe a) -> Value -> A.Parser a
parseStringMarker marker parseFunc = withObject "Object expected" $ \o ->
        (parseFunc <$> o .: marker) >>= maybeParse
    where maybeParse Nothing = empty
          maybeParse (Just v) = pure v

formatStringMarker :: T.Text -> (a -> String) -> a -> Value
formatStringMarker marker formatFunc val = object [marker .= formatFunc val]

instance ToJSON ObjectPath where
    toJSON = formatStringMarker "__object_path" formatObjectPath

instance FromJSON ObjectPath where
    parseJSON = parseStringMarker "__object_path" parseObjectPath

instance ToJSON InterfaceName where
    toJSON = toJSON . formatInterfaceName

instance ToJSON MemberName where
    toJSON = toJSON . formatMemberName

instance ToJSON Signature where
    toJSON = toJSON . formatSignature

instance ToJSON Signal where
    toJSON s = object [ "path"   .= toJSON (signalPath s)
                      , "iface"  .= toJSON (signalInterface s)
                      , "member" .= toJSON (signalMember s)
                      , "body"   .= toJSON (signalBody s)
                      ]

instance ToJSON ErrorName where
    toJSON = toJSON . formatErrorName

instance ToJSON Serial where
    toJSON = toJSON . serialValue

instance ToJSON BusName where
    toJSON = toJSON . formatBusName

instance ToJSON MethodError where
    toJSON e = object [ "name"        .= toJSON (methodErrorName e)
                      , "serial"      .= toJSON (methodErrorSerial e)
                      , "sender"      .= toJSON (methodErrorSender e)
                      , "destination" .= toJSON (methodErrorDestination e)
                      , "body"        .= toJSON (methodErrorBody e)
                      , "message"     .= toJSON (methodErrorMessage e)
                      ]

instance ToJSON MethodReturn where
    toJSON r = object [ "serial"      .= toJSON (methodReturnSerial r)
                      , "sender"      .= toJSON (methodReturnSender r)
                      , "destination" .= toJSON (methodReturnDestination r)
                      , "body"        .= toJSON (methodReturnBody r)
                      ]
