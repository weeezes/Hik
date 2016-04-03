{-# LANGUAGE OverloadedStrings #-}

module Actions
    (
        ircPingPong,
        ircQuit,
        ircEcho
    ) where
        
import qualified Data.Text as T
import Types as IT

ircPingPong _ msg =
    if "PING :" `T.isPrefixOf` msg then
        let (_,n) = T.breakOn ":" msg in 
            Just $ T.concat ["PONG ", n]
    else
        Nothing

ircQuit _ msg =
    if "!quit" `T.isInfixOf` msg then
        Just "QUIT :byebye!\r\n"
    else
        Nothing

ircEcho c msg =
    if "!echo" `T.isInfixOf` msg then
        let (_, msg') = T.breakOnEnd "!echo " msg in
            Just $ T.concat ["PRIVMSG ", T.pack . IT.channelName $ c, " :", msg']
    else
        Nothing
        