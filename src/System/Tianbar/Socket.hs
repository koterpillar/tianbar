module System.Tianbar.Socket where

-- Socket connectivity

import Network.Socket
import Network.URI

import System.Tianbar.Plugin

data SocketPlugin = SocketPlugin

instance Plugin SocketPlugin where
    simpleInitialize = SocketPlugin

    handleRequest SocketPlugin = withScheme "socket:" $ \uri -> do
        let socketPath = uriPath uri
        let params = parseQuery uri
        let Just request = lookupQueryParam "request" params
        sock <- socket AF_UNIX Stream defaultProtocol
        connect sock $ SockAddrUnix socketPath
        -- TODO: replace with ByteStrings
        -- TODO: resend data until done
        _ <- send sock request
        shutdown sock ShutdownSend
        response <- recv sock 4096
        close sock
        returnContent response
