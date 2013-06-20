{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main (main) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Czar.Log
import Czar.Socket
import Network.BSD              hiding (hostName)
import Options

import Czar.Protocol


import qualified System.ZMQ3.Monadic as Z

import qualified Control.Exception     as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T

import qualified Data.Sequence as Seq

defineOptions "HandlerOpts" $ do
    stringOption "hdServer" "server" defaultHandler "Server for the handler to connect to."

main :: IO ()
main = runCommand $ \HandlerOpts{..} _ -> runScript $ do
    setLogging

    uri <- parseUri hdServer

    logInfo "Starting handler ..."

    tryCatchIO (cleanup uri) $ runZMQ $ do
        sock <- socket Dealer

        connect sock $ show uri

        -- identify name of handler
        setIdentity (restrict "graphite") sock

        let subs = Subscription
                      "graphite"
                      (Just "Graphite Handler")
                      (Seq.fromList [Tag "*"])

        -- send subscription
        send sock [] $ messagePut subs

        -- setup handler to respond to heartbeats with identity
        forever $ do
            bs <- receive sock
            liftIO $ print bs

        -- setup handler to handle metrics
