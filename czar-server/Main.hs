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
import qualified Control.Concurrent.Timeout          as Timeout
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

defineOptions "Server" $ do
    addressOption "optAgents" "listen" defaultServer
        "Listen address for Czar Agent connections"

    addressOption "optHandlers" "publish" defaultHandler
        "Listen address for Handler connections"

    secondsOption "optTimeout" "timeout" 60
        "Timeout for heartbeat responses before terminating a connection"

    emissionOption

    debugOption

main :: IO ()
main = runCommand $ \Server{..} _ -> do
    setLogging optDebug

    logInfo "starting server ..."

    routes <- liftIO emptyRoutes
    stats  <- newStats
        "localhost"
        "czar.server.internal"
        Nothing
        ["czar-server"]

    raceAll
        [ healthCheck optEmission stats $ flip notify routes
        , listenHandlers optTimeout optHandlers routes
        , listenAgents optTimeout optAgents routes
        ]

-- FIXME: Error or wipe old subscription if a handler sends it twice

listenHandlers :: MonadCatchIO m => Seconds -> Address -> Routes ThreadId -> m ()
listenHandlers n addr routes =
    listen addr $ logPeerRX "accepting handler" >> receive handshake
  where
    handshake (S sub@S.Subscription{..}) = do
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

        continue timer `finally` liftIO (killThread child)

    handshake _ = return ()

    continue = receive . yield

    yield t (E evt) = Timeout.reset t >> notify evt routes >> continue t
    yield t Ack     = logPeerRX "ACK" >> Timeout.reset t >> continue t
    yield t _       = logPeerRX "FIN" >> Timeout.cancel t

listenAgents :: MonadCatchIO m => Seconds -> Address -> Routes ThreadId -> m ()
listenAgents n addr routes =
    listen addr $ logPeerRX "accepting agent" >> heartbeat n >>= continue
  where
    continue = receive . yield

    yield t (E evt) = Timeout.reset t >> notify evt routes >> continue t
    yield t Ack     = logPeerRX "ACK" >> Timeout.reset t >> continue t
    yield t _       = logPeerRX "FIN" >> Timeout.cancel t
