{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main (main) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Czar.Log
import Czar.Socket
import Network.BSD              hiding (hostName)
import Options
import System.ZMQ3.Monadic      hiding (async)

import qualified Control.Exception     as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T

defineOptions "ServerOpts" $ do
    stringOption "srvListen"  "listen"  "tcp://127.0.0.1:5555" ""
    stringOption "srvPublish" "publish" "ipc://czar-server.sock" ""

main :: IO ()
main = runCommand $ \ServerOpts{..} _ -> runScript $ do
    setLogging

    luri <- parseUri srvListen
    puri <- parseUri srvPublish
    chan <- liftIO $ atomically newTChan

    logInfo "Starting server ..."

    tryCatchS (cleanup luri) $ do
        l <- listen luri chan
        p <- publish puri chan

        waitEither_ l p
  where
    listen uri chan = async $ runZMQ $ do
        sock <- serverListen uri
        forever $ do
            bs <- receive sock
            logInfo $ "Received " ++ BS.unpack bs ++ " on " ++ show uri
            liftIO . atomically $ writeTChan chan bs

    publish uri chan = async $ runZMQ $ do
        sock <- serverPublish uri
        forever $ do
            bs <- liftIO . atomically $ readTChan chan
            send sock [] bs
            logInfo $ "Sent " ++ BS.unpack bs ++ " to " ++ show uri
