module System.Tianbar.XMonadLog ( tianbarPP
                                , dbusLog
                                , dbusLogWithPP
                                ) where

import Codec.Binary.UTF8.String (decodeString)

import Data.Char
import Data.List
import Data.Maybe

import DBus
import DBus.Client

import XMonad
import XMonad.Hooks.DynamicLog

wrapClass :: String -> String -> String
wrapClass cls = wrap ("<span class='" ++ cls ++ "'>") "</span>"

wrapWorkspaceClass :: String -> String -> String
wrapWorkspaceClass cls wid = wrapClass classes wid
    where classes = intercalate " " [cls, ws, ws ++ wid]
          ws = "workspace"

escapeHtml :: String -> String
escapeHtml = concatMap escapeChar
    where escapeChar '\"' = "&#34;"
          escapeChar '\'' = "&#39;"
          escapeChar '&'  = "&#38;"
          escapeChar '<'  = "&#60;"
          escapeChar '>'  = "&#62;"
          escapeChar c | ord c >= 160 = "&#" ++ show (ord c) ++ ";"
                       | otherwise    = [c]

tianbarPP :: PP
tianbarPP = defaultPP { ppTitle = escapeHtml
                      , ppCurrent = wrapWorkspaceClass "current"
                      , ppVisible = wrapWorkspaceClass "visible"
                      , ppUrgent = wrapWorkspaceClass "urgent"
                      , ppHidden = wrapWorkspaceClass "hidden"
                      , ppHiddenNoWindows = wrapWorkspaceClass "hidden empty"
                      , ppSep = ""
                      , ppOrder = zipWith wrapClass [ "workspaces"
                                                    , "layout"
                                                    , "title"
                                                    ]
                      }

dbusLogWithPP :: Client -> PP -> X ()
dbusLogWithPP client pp = dynamicLogWithPP pp { ppOutput = outputThroughDBus client }

-- | A DBus-based logger with a default pretty-print configuration
dbusLog :: Client -> X ()
dbusLog client = dbusLogWithPP client tianbarPP

sig :: Signal
sig = signal (fromJust $ parseObjectPath "/org/xmonad/Log")
             (fromJust $ parseInterfaceName "org.xmonad.Log")
             (fromJust $ parseMemberName "Update")

outputThroughDBus :: Client -> String -> IO ()
outputThroughDBus client str = do
    -- The string that we get from XMonad here isn't quite a normal
    -- string - each character is actually a byte in a utf8 encoding.
    -- We need to decode the string back into a real String before we
    -- send it over dbus.
    let str' = decodeString str
    emit client sig { signalBody = [ toVariant str' ] }
