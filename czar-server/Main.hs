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

import           Control.Concurrent       hiding (yield)
import           Control.Concurrent.STM
import           Control.Error
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.IO.Class
import           Network.Socket           (SockAddr)
import           Options

import           Control.Concurrent.Race
import           Control.Concurrent.Timer (Seconds)
import           Czar.EKG
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

        healthCheck

        linkedRace_
            (listenHandlers n hds routes)
            (listenAgents n lst)

listenHandlers :: MonadCatchIO m => Seconds -> SockAddr -> Routes ThreadId -> m ()
listenHandlers n addr routes = listen addr $ receive yield
  where
    yield (S sub) = do
        logPeerInfo $ "subscribing to " ++ formatTags sub

        parent <- liftIO myThreadId
        queue  <- liftIO $ subscribe sub parent routes

         -- publish events in forked child
        child  <- fork $ liftIO (atomically $ readTQueue queue) >>= send
        timer  <- heartbeat n $ unsubscribe parent routes

        -- block on keepalive in the main thread
        keepalive timer `finally` liftIO (killThread child)

    yield _ = return ()

    keepalive = receive . ack

    ack t Ack = logPeerRX "ACK" >> Timer.reset t >> keepalive t
    ack t _   = logPeerRX "FIN" >> Timer.cancel t

listenAgents :: MonadCatchIO m => Seconds -> SockAddr -> m ()
listenAgents n addr = listen addr $ heartbeat n (return ()) >>= continue
  where
    continue = receive . yield

    yield t (E evt) = Timer.reset t >> liftIO (print evt) >> continue t
    yield t Ack     = logPeerRX "ACK" >> Timer.reset t >> continue t
    yield t _       = logPeerRX "FIN" >> Timer.cancel t

healthCheck :: (Functor m, MonadCatchIO m) => m ThreadId
healthCheck = liftIO . forkIO $ do
    stats <- newStats
    forever $ do
        threadDelay 10000000
        m <- sampleStats stats
        putStrLn "Stats:"
        print m

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
--  Sends: ACK | EVT
--  Receives: SYN | EVT
