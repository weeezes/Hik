{-# LANGUAGE OverloadedStrings #-}
module Main where

import Bot
import Types as IT
import Actions
import Data.ConfigFile
import Data.Either.Utils
import Control.Monad.Except

data Config = 
    Config {
        server :: String,
        port :: Int,
        botnick :: String,
        botname :: String,
        channel :: String   
    } deriving (Show)

opts = runExceptT $ do
        cp <- join $ liftIO $ readfile emptyCP "./configurations/config.txt"

        server <- get cp "DEFAULT" "server"
        port <- get cp "DEFAULT" "port"
        botnick <- get cp "DEFAULT" "botnick"
        botname <- get cp "DEFAULT" "botname"
        channel <- get cp "DEFAULT" "botchannel"
        
        return $ Config server port botnick botname channel

main :: IO ()
main =
    do
      config <- opts
      
      case config of
          Right conf -> do
            let s = IT.Server (server conf) 6667
            let u = IT.User (botnick conf) (botname conf)
            let c = IT.Channel (channel conf)
            
            conn <- openConnection s u c

            startBot conn [ircPingPong, ircQuit, ircEcho] ""
            
            putStrLn "Killing bot..."
          _ -> print "Couldn\'t parse configurations."