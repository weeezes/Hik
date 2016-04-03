module Types 
    (
        Server (Server, address, port),
        User (User, nick, realName),
        Channel (Channel, channelName),
        Connection (Connection, socket, connectionChannel),
        IRCAction
    ) where

import Network.Socket
import qualified Data.Text as T

data Server = Server {
        address :: String,
        port :: Int
    }

data User = User {
        nick :: String,
        realName :: String
    }

data Channel = Channel {
        channelName :: String
    }
    
data Connection = Connection {
        socket :: Socket,
        connectionChannel :: Channel
    } | IRCClosed

type IRCAction = Channel -> T.Text -> Maybe T.Text

