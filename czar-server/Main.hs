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

import Control.Concurrent      (ThreadId, myThreadId)
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

    integerOption "srvTimeout" "heartbeat" 5
        ""

    boolOption "srvVerbose" "verbose" False
        "Be really loud."

main :: IO ()
main = runCommand $ \ServerOpts{..} _ -> scriptLogging srvVerbose $ do
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
    logPeer INFO "Accepted"
    receive yield
  where
    yield (S sub) = do
        tid   <- liftIO myThreadId
        queue <- liftIO $ subscribe sub tid routes
        _     <- sendHeartbeats n >>= fork . keepalive

        forever $ do
            evt <- liftIO . atomically $ readTQueue queue
            send evt
    yield _       = return ()

    keepalive = receive . heartbeat

    heartbeat t Ack = logPeerRX "ACK" >> resetTimer t >> keepalive t
    heartbeat t _   = logPeerRX "FIN" >> cancelTimer t

listenAgents :: MonadCatchIO m => Seconds -> SockAddr -> m ()
listenAgents n addr = listen addr $ do
    logPeer INFO "ACCEPT"
    sendHeartbeats n >>= continue
  where
    continue = receive . yield

    yield t (E evt) = resetTimer t >> liftIO (print evt) >> continue t
    yield t Ack     = logPeerRX "ACK" >> resetTimer t >> continue t
    yield t _       = logPeerRX "FIN" >> cancelTimer t
