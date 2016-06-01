module System.Tianbar.Plugin.All (
    AllPlugins
) where

import System.Tianbar.Plugin.Combined
import System.Tianbar.Plugin.DBus
import System.Tianbar.Plugin.ExecuteCommand
import System.Tianbar.Plugin.FileSystem
import System.Tianbar.Plugin.GSettings
import System.Tianbar.Plugin.Socket

type AllPlugins =
    Combined DBusPlugin (
        Combined ExecuteCommand (
            Combined FileSystem (
                Combined GSettings (
                    Combined SocketPlugin (
                        Combined EmptyPlugin EmptyPlugin)))))
