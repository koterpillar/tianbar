{-# Language OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Tianbar.DBus.JSON () where

import Data.Aeson hiding (Array)
import Data.Int
import qualified Data.Map as M
import Data.Word

import DBus

instance ToJSON Variant where
    toJSON v = case variantType v of
        TypeBoolean -> let Just b = fromVariant v :: Maybe Bool in toJSON b
        TypeWord8 -> let Just i = fromVariant v :: Maybe Word8 in toJSON i
        TypeWord16 -> let Just i = fromVariant v :: Maybe Word16 in toJSON i
        TypeWord32 -> let Just i = fromVariant v :: Maybe Word32 in toJSON i
        TypeWord64 -> let Just i = fromVariant v :: Maybe Word64 in toJSON i
        TypeInt16 -> let Just i = fromVariant v :: Maybe Int16 in toJSON i
        TypeInt32 -> let Just i = fromVariant v :: Maybe Int32 in toJSON i
        TypeInt64 -> let Just i = fromVariant v :: Maybe Int64 in toJSON i
        TypeDouble -> let Just i = fromVariant v :: Maybe Double in toJSON i
        TypeString -> let Just s = fromVariant v :: Maybe String in toJSON s

        TypeSignature -> let Just s = fromVariant v :: Maybe Signature in
            toJSON $ formatSignature s
        TypeObjectPath -> let Just p = fromVariant v :: Maybe ObjectPath in
            toJSON $ formatObjectPath p

        TypeVariant -> let Just n = fromVariant v :: Maybe Variant in toJSON n
        TypeArray _ -> let Just a = fromVariant v :: Maybe Array in
            toJSON $ arrayItems a
        TypeDictionary _ _ -> let Just d = fromVariant v :: Maybe Dictionary in
            toJSON $ M.fromList $ map variantStringKey $ dictionaryItems d
        TypeStructure _ -> let Just a = fromVariant v :: Maybe Structure in
            toJSON $ structureItems a

variantString :: Variant -> String
variantString v = s
    where Just s = case variantType v of
                       TypeString -> fromVariant v
                       TypeVariant -> Just $ variantString v'
                           where Just v' = fromVariant v
                       _ -> Just $ show v

variantStringKey :: (Variant, Variant) -> (String, Variant)
variantStringKey (k, v) = (variantString k, v)

instance ToJSON ObjectPath where
    toJSON = toJSON . formatObjectPath

instance ToJSON InterfaceName where
    toJSON = toJSON . formatInterfaceName

instance ToJSON MemberName where
    toJSON = toJSON . formatMemberName

instance ToJSON Signal where
    toJSON s = object [ "path"   .= toJSON (signalPath s)
                      , "iface"  .= toJSON (signalInterface s)
                      , "member" .= toJSON (signalMember s)
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
