{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.IO.Class
import           Czar.EKG
import qualified Czar.Internal.Protocol.Subscription as S
import           Czar.Log
import           Czar.Options
import           Czar.Protocol
import           Czar.Server.Routing
import           Czar.Socket
import           Czar.Types
import           Data.Foldable                       (toList)

data Server = Server
    { optAgents   :: Address
    , optHandlers :: Address
    , optTimeout  :: Seconds
    , optMetrics  :: Seconds
    , optDebug    :: Bool
    }

instance CommonOptions Server where
    debug = optDebug

program :: ParserInfo Server
program = info (helper <*> parser) $
       fullDesc
    <> progDesc "Start the Czar Server"
    <> header "czar-server - a test for optparse-applicative"
  where
    parser = Server
        <$> addressOption "listen" defaultServer
                "Listen address for Czar Agent connections"

        <*> addressOption "publish" defaultHandler
                "Listen address for Handler connections"

        <*> secondsOption "timeout" 60
                "Timeout for heartbeat responses before terminating a connection"

        <*> secondsOption "metric-interval" 30
                "Interval between internal metric emissions"

        <*> debugSwitch

main :: IO ()
main = runProgram program $ \Server{..} -> do
    logInfo "starting server ..."

    routes <- liftIO emptyRoutes
    stats  <- newStats
        "localhost"
        "czar.server.internal"
        Nothing
        ["czar-server"]

    raceAll
        [ healthCheck optMetrics stats $ flip notify routes
        , listenHandlers optTimeout optHandlers routes
        , listenAgents optTimeout optAgents routes
        ]

listenHandlers :: MonadCatchIO m => Seconds -> Address -> Routes ThreadId -> m ()
listenHandlers n addr routes =
    listen addr $ logPeerRX "accepting handler" >> receive yield
  where
    yield (S sub@S.Subscription{..}) = do
        logPeerInfo $ "subscribing "
            ++ uToString identity
            ++ " handler to "
            ++ show (toList tags)

        parent <- liftIO myThreadId
        queue  <- liftIO $ subscribe sub parent routes
        timer  <- heartbeat n

        child  <- forkContextFinally
            (forever $ do
                evt <- liftIO . atomically $ readTQueue queue
                send evt)
            (do logInfo $ "unsubscribing " ++ show parent
                unsubscribe parent routes)

        keepalive timer `finally` liftIO (killThread child)

    yield _ = return ()

    keepalive = receive . ack

    ack t Ack = logPeerRX "ACK" >> Timer.reset t >> keepalive t
    ack t _   = logPeerRX "FIN" >> Timer.cancel t

listenAgents :: MonadCatchIO m => Seconds -> Address -> Routes ThreadId -> m ()
listenAgents n addr routes =
    listen addr $ logPeerRX "accepting agent" >> heartbeat n >>= continue
  where
    continue = receive . yield

    yield t (E evt) = Timer.reset t >> notify evt routes >> continue t
    yield t Ack     = logPeerRX "ACK" >> Timer.reset t >> continue t
    yield t _       = logPeerRX "FIN" >> Timer.cancel t
