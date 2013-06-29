{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import Control.Concurrent
import Control.Concurrent.Race
import Control.Concurrent.STM
import Control.Concurrent.Timer
import Control.Error
import Control.Monad
import Control.Monad.CatchIO
import Control.Monad.IO.Class
import Network.Socket           (SockAddr)
import Options

import Czar.Log
import Czar.Protocol
import Czar.Server.Routing
import Czar.Socket

-- FIXME: Add a --force command to overwrite unix sockets if possible

defineOptions "ServerOpts" $ do
    stringOption "srvListen" "listen" defaultServer
        ""

    stringOption "srvHandlers" "publish" defaultHandler
        ""

    integerOption "srvTimeout" "heartbeat" 30
        ""

main :: IO ()
main = runCommand $ \ServerOpts{..} _ -> scriptLogging $ do
    hds <- parseAddr srvHandlers
    lst <- parseAddr srvListen

    logInfo "Starting server ..."

    let n = fromInteger srvTimeout

    scriptIO $ do
        routes <- liftIO emptyRoutes
        linkedRace_
            (listenHandlers n hds routes)
            (listenAgents n lst)

listenHandlers :: MonadCatchIO m => Seconds -> SockAddr -> Routes ThreadId -> m ()
listenHandlers n addr routes = listen addr $ do
    logInfo "Accepted handler connection"
    receive handle
  where
    handle (S sub) = do
        tid   <- liftIO myThreadId
        queue <- liftIO $ subscribe sub tid routes

        timer <- sendHeartbeats n
        peer  <- peerName
        _     <- fork $ heartbeats peer timer

        forever $ do
            evt <- liftIO . atomically $ readTQueue queue
            send evt
    handle _       = return ()

    heartbeats p = receive . heart p

    heart p t Ack = logInfo ("ACK <- " ++ p) >> resetTimer t >> heartbeats p t
    heart p t _   = logWarn ("UNKNOWN/FIN -> " ++ p) >> cancelTimer t

listenAgents :: MonadCatchIO m => Seconds -> SockAddr -> m ()
listenAgents n addr = listen addr $ do
    peer <- peerName
    logInfo $ "ACCEPT <- " ++ peer

    sendHeartbeats n >>= loop peer
  where
    loop p = receive . handle p

    handle p t (E evt) = resetTimer t >> liftIO (print evt) >> loop p t
    handle p t Ack     = logInfo ("ACK <- " ++ p) >> resetTimer t >> loop p t
    handle p t _       = logWarn ("UNKNOWN/FIN -> " ++ p) >> cancelTimer t
