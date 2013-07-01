{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

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

import           Control.Concurrent                  hiding (yield)
import           Control.Concurrent.Race
import           Control.Concurrent.STM
import qualified Control.Concurrent.Timer            as Timer
import           Control.Error
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.IO.Class
import           Czar.EKG
import qualified Czar.Internal.Protocol.Subscription as S
import           Czar.Log
import           Czar.Protocol
import           Czar.Server.Routing
import           Czar.Socket
import           Data.Foldable                       (toList)
import           Network.Socket                      (SockAddr)
import           Options

defineOptions "ServerOpts" $ do
    stringOption "srvAgents" "listen-agents" defaultServer
        ""

    stringOption "srvHandlers" "listen-handlers" defaultHandler
        ""

    integerOption "srvHeartbeat" "heartbeat" 300
        ""

    boolOption "srvVerbose" "verbose" False
        "Be really loud."

main :: IO ()
main = runCommand $ \ServerOpts{..} _ -> scriptLogging srvVerbose $ do
    hds <- parseAddr srvHandlers
    lst <- parseAddr srvAgents

    logInfo "starting server ..."

    -- seconds to microseconds
    let timeout = fromIntegral srvHeartbeat * 1000000
        metrics = 30 * 1000000

    scriptIO $ do
        routes <- liftIO emptyRoutes
        stats  <- newStats
            "localhost"
            "czar.server.internal"
            Nothing
            ["czar-server"]

        healthCheck metrics routes stats

        linkedRace_
            (listenHandlers timeout hds routes)
            (listenAgents timeout lst routes)

listenHandlers :: MonadCatchIO m => Int -> SockAddr -> Routes ThreadId -> m ()
listenHandlers n addr routes = listen addr $ receive yield
  where
    yield (S sub@S.Subscription{..}) = do

        logPeerInfo $ "subscribing "
            ++ uToString identity
            ++ " handler to "
            ++ show (toList tags)

        parent <- liftIO myThreadId
        queue  <- liftIO $ subscribe sub parent routes

        timer  <- heartbeat n

        -- there is a race here which forkFinally would fix,
        -- when the thread fails to start the subscription is not removed
        child  <- fork $ finally
            (forever $ do
                evt <- liftIO . atomically $ readTQueue queue
                send evt)
            (do
                logInfo $ "unsubscribing " ++ show parent
                unsubscribe parent routes)

        -- block on keepalive in the main thread
        keepalive timer `finally` liftIO (killThread child)

    yield _ = return ()

    keepalive = receive . ack

    ack t Ack = logPeerRX "ACK" >> Timer.reset t >> keepalive t
    ack t _   = logPeerRX "FIN" >> Timer.cancel t

listenAgents :: MonadCatchIO m => Int -> SockAddr -> Routes ThreadId -> m ()
listenAgents n addr routes = listen addr $ heartbeat n >>= continue
  where
    continue = receive . yield

    yield t (E evt) = Timer.reset t >> notify evt routes >> continue t
    yield t Ack     = logPeerRX "ACK" >> Timer.reset t >> continue t
    yield t _       = logPeerRX "FIN" >> Timer.cancel t

healthCheck :: (Functor m, MonadCatchIO m)
            => Int
            -> Routes ThreadId
            -> Stats
            -> m ThreadId
healthCheck n routes stats = liftIO . forkIO . forever $ do
    threadDelay n
    logDebug "sampling internal stats"
    sampleStats stats >>= flip notify routes

-- Each socket type flips between send and receive depending on a prior state

AgentServer Push Pong
 Handshakes with ()
 Sends: Few ACK, Many EVT
 Receives: SYN

ServerAgent Pull Ping
 Handshakes with ()
 Sends: SYN
 Receives: ACK | EVT


AgentAgent Push Pong
 Handshakes with ()
 Sends: Few ACK, Many EVT
 Receives: SYN

AgentAgent Pull Ping
 Handshakes with ()
 Sends: SYN
 Receives ACK | EVT


ServerHandler Push Ping
 Handshakes with SUB
 Sends: SYN | EVT
 Receives: ACK

HandlerServer Pull Pong
 Handshakes with Sub
 Sends: ACK | EVT
 Receives: SYN | EVT
