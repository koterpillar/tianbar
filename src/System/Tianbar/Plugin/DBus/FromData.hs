{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Tianbar.Plugin.DBus.FromData () where

import Control.Monad

import Data.List.Split
import Data.Maybe

import DBus
import DBus.Client

import System.Tianbar.Plugin

instance FromData MatchRule where
    fromData = do
        objPath <- fromData
        iface <- fromData
        memberName <- fromData
        return $ matchAny { matchSender = Nothing
                          , matchDestination = Nothing
                          , matchPath = objPath
                          , matchInterface = iface
                          , matchMember = memberName
                          }

instance FromData MethodCall where
    fromData = do
        callPath <- fromData
        iface <- fromData
        member <- fromData
        callBody <- liftM (map variantFromString) $ looks "body[]"
        dest <- fromData
        let setBodyDest mcall = mcall { methodCallBody = callBody
                                      , methodCallDestination = dest
                                      }
        return $ setBodyDest $ methodCall callPath iface member

instance FromData ObjectPath where
    fromData = liftM (fromJust . parseObjectPath) $ look "path"

instance FromData InterfaceName where
    fromData = liftM (fromJust . parseInterfaceName) $ look "iface"

instance FromData MemberName where
    fromData = liftM (fromJust . parseMemberName) $ look "member"

instance FromData BusName where
    fromData = liftM (fromJust . parseBusName) $ look "destination"

variantFromString :: String -> Variant
variantFromString param = case splitOn ":" param of
    ["string", str] -> toVariant str
    _ -> error $ "Invalid variant string: " ++ show param
