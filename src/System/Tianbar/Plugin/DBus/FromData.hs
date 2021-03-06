{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Tianbar.Plugin.DBus.FromData () where

import Data.Aeson
import Data.ByteString.Lazy as LBS
import Data.Maybe

import DBus
import DBus.Client

import System.Tianbar.Plugin
import System.Tianbar.Plugin.DBus.JSON ()

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
        callBody <- (fromJust . decode . LBS.fromStrict) <$> lookBS "body"
        dest <- fromData
        let setBodyDest mcall = mcall { methodCallBody = callBody
                                      , methodCallDestination = dest
                                      }
        return $ setBodyDest $ methodCall callPath iface member

instance FromData ObjectPath where
    fromData = fromJust . parseObjectPath <$> look "path"

instance FromData InterfaceName where
    fromData = fromJust . parseInterfaceName <$> look "iface"

instance FromData MemberName where
    fromData = fromJust . parseMemberName <$> look "member"

instance FromData BusName where
    fromData = fromJust . parseBusName <$> look "destination"
