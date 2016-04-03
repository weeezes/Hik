{-# LANGUAGE OverloadedStrings #-}

module Bot
    ( openConnection,
      startBot
    ) where

import Network.Socket
import Data.String
import Control.Monad
import qualified Data.Maybe as MB
import qualified Data.Text as T
import Actions
import qualified Types as IT

openConnection :: IT.Server -> IT.User -> IT.Channel -> IO IT.Connection
openConnection server user channel =
    do
      let address = IT.address server
      let port = show . IT.port $ server
        
      sock <- openlog address port
        
      send sock $ "NICK " ++ IT.nick user ++ "\r\n"
      send sock $ "USER " ++ IT.nick user ++ " " ++ address ++ " arb: " ++ IT.realName user ++ "\r\n"
      send sock $ "JOIN " ++ IT.channelName channel ++ "\r\n"
      
      return $ IT.Connection sock channel

handleMessage :: IT.Connection -> [IT.IRCAction] -> T.Text -> IO ()
handleMessage connection actions message =
    do
        let channel = IT.connectionChannel connection
        let handled = T.unpack <$> MB.mapMaybe (\a -> a channel message) actions
        let sock = IT.socket connection
        
        mapM_ (send sock) handled
        
startBot :: IT.Connection  -> [IT.IRCAction] -> T.Text -> IO ()
startBot connection actions buffer =
    do
        print buffer
        
        let sock = IT.socket connection
        let buf_lines = T.lines buffer
        
        mapM_ (handleMessage connection actions) buf_lines
        
        rd <- recv sock 1024
        
        let (_,unhandled) = T.breakOnEnd "\r\n" buffer
        let new_buffer = T.concat [unhandled, T.pack rd]
        
        startBot connection actions new_buffer

openlog hostname port =
    do 
      addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
      let serveraddr = head addrinfos

      sock <- socket (addrFamily serveraddr) Stream defaultProtocol

      setSocketOption sock KeepAlive 1

      connect sock (addrAddress serveraddr)

      return sock
       