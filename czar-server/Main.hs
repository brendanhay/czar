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

import           Control.Concurrent       (ThreadId, myThreadId)
import           Control.Concurrent.STM
import           Control.Error
import           Control.Monad.CatchIO
import           Control.Monad.IO.Class
import           Network.Socket           (SockAddr)
import           Options

import           Control.Concurrent.Race
import           Control.Concurrent.Timer (Seconds)
import           Czar.Log
import           Czar.Protocol
import           Czar.Server.Routing
import           Czar.Socket

import qualified Control.Concurrent.Timer as Timer

defineOptions "ServerOpts" $ do
    stringOption "srvListen" "listen" defaultServer
        ""

    stringOption "srvHandlers" "publish" defaultHandler
        ""

    integerOption "srvHeartbeat" "heartbeat" 5
        ""

    boolOption "srvVerbose" "verbose" False
        "Be really loud."

main :: IO ()
main = runCommand $ \ServerOpts{..} _ -> scriptLogging srvVerbose $ do
    hds <- parseAddr srvHandlers
    lst <- parseAddr srvListen

    logInfo "starting server ..."

    let n = fromInteger srvHeartbeat

    scriptIO $ do
        routes <- liftIO emptyRoutes
        linkedRace_
            (listenHandlers n hds routes)
            (listenAgents n lst)

listenHandlers :: MonadCatchIO m => Seconds -> SockAddr -> Routes ThreadId -> m ()
listenHandlers n addr routes = listen addr $ receive yield `finally` logPeerRX "FIN"
  where
    yield (S sub) = do
        tid   <- liftIO myThreadId

        logPeerInfo $ "subscribing to " ++ formatTags sub

        queue <- liftIO $ subscribe sub tid routes

         -- publish events in forked child
        fork $ do
            evt <- liftIO . atomically $ readTQueue queue
            send evt

        -- keepalive in the main thread
        sendHeartbeats n >>= keepalive

    yield _ = return ()

    keepalive = receive . heartbeat

    heartbeat t Ack = logPeerRX "ACK" >> Timer.reset t >> keepalive t
    heartbeat t _   = logPeerRX "FIN" >> Timer.cancel t

listenAgents :: MonadCatchIO m => Seconds -> SockAddr -> m ()
listenAgents n addr = listen addr $ sendHeartbeats n >>= continue
  where
    continue = receive . yield

    yield t (E evt) = Timer.reset t >> liftIO (print evt) >> continue t
    yield t Ack     = logPeerRX "ACK" >> Timer.reset t >> continue t
    yield t _       = logPeerRX "FIN" >> Timer.cancel t


-- Each socket type flips between send and receive depending on a prior state

-- AgentServer Push Pong
--  Handshakes with ()
--  Sends: Few ACK, Many EVT
--  Receives: SYN

-- ServerAgent Pull Ping
--  Handshakes with ()
--  Sends: SYN
--  Receives: ACK | EVT


-- AgentAgent Push Pong
--  Handshakes with ()
--  Sends: Few ACK, Many EVT
--  Receives: SYN

-- AgentAgent Pull Ping
--  Handshakes with ()
--  Sends: SYN
--  Receives ACK | EVT


-- ServerHandler Push Ping
--  Handshakes with SUB
--  Sends: SYN | EVT
--  Receives: ACK

-- HandlerServer Pull Pong
--  Handshakes with Sub
--  Sends: ACK
--  Receives: SYN | EVT
