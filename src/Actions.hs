{-# LANGUAGE OverloadedStrings #-}

module Actions
    (
        pingPong,
        quit,
        echo
    ) where
        
import qualified Data.Text as T
import Types as IT

pingPong _ msg =
    if "PING :" `T.isPrefixOf` msg then
        let (_,n) = T.breakOn ":" msg in 
            Just $ T.concat ["PONG ", n]
    else
        Nothing

quit _ msg =
    if "!quit" `T.isInfixOf` msg then
        Just "QUIT :byebye!\r\n"
    else
        Nothing

echo c msg =
    if "!echo" `T.isInfixOf` msg then
        let (_, msg') = T.breakOnEnd "!echo " msg in
            Just $ T.concat ["PRIVMSG ", T.pack . IT.channelName $ c, " :", msg']
    else
        Nothing
        