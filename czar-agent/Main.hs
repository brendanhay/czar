{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import           Control.Concurrent           hiding (yield)
import           Control.Concurrent.Race
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.IO.Class
import           Czar.EKG
import qualified Czar.Internal.Protocol.Event as E
import           Czar.Log
import           Czar.Options
import           Czar.Protocol                hiding (S, fromString)
import           Czar.Socket
import           Czar.Types
import qualified Data.Sequence                as Seq
import           Data.String
import           Network.BSD                  hiding (hostName)

commonOptions "Main" $ return ()

defineOptions "Send" $ do
    addressOption "sendAgent" "agent" defaultAgent
        "Czar Agent address to connect to"

    stringOption "sendType" "type" ""
        "Type of the metric to send"

    stringOption "sendKey" "key" "oneshot.event"
        ""

    stringOption "sendValue" "value" ""
        ""

    doubleOption "sendWarnLower" "warning-lower" 0
        "Lower bound for warnings"

    doubleOption "sendWarnUpper" "warning-upper" 100
        "Upper bound for warnings"

    doubleOption "sendCritLower" "critical-lower" 0
        "Lower bound for criticals"

    doubleOption "sendCritUpper" "critical-upper" 1000
        "Upper bound for criticals"

    stringsOption "sendDesc" "description" []
        "Decsription of the event"

    stringsOption "sendTags" "tags" []
        "Comma separated list to tags to annotate the event"

defineOptions "Connect" $ do
    addressOption "connListen" "listen" defaultAgent
        "Listen address for Czar Agent connections"

    addressOption "connServer" "server" defaultServer
        "Czar Server address to connect to"

    maybeStringOption "connHost" "host"
        "Hostname used to identify the machine"

    secondsOption "connMetrics" "metrics-interval" 30
        "Interval between internal metric emissions"

    secondsOption "connSplay" "splay" 1
        "Stagger check start times by this number of seconds"

    stringsOption "connTags" "tags" []
        "Comma separated list of tags to prepend to every event"

    secondsOption "optEmission" "metric-frequency" 30
        "Frequency of internal metric emissions"

main :: IO ()
main = runSubcommand
    [ command "send" send' :: Subcommand Main (IO ())
    , command "connect" connect' :: Subcommand Main (IO ())
    ]
  where
    send' Send{..} = do
        logInfo "connecting to agent ..."

        connect sendAgent $ do
            send $ E.Event 0 "hi!" "key" Nothing (Seq.fromList []) (Seq.fromList []) (Seq.fromList [])
            logInfo $ "payload sent to " ++ show sendAgent

        logInfo "Done."

    connect' Connect{..} = do
        host  <- maybe (liftIO getHostName) return connHost

        logInfo $ "identifying host as " ++ host
        logInfo "starting agent ..."

        queue <- atomically newTQueue
        stats <- newStats
            (fromString host)
            "czar.agent"
            Nothing
            ["czar-agent"]

        raceAll
            [ healthCheck optEmission stats (atomically . writeTQueue queue)
            , connectServer connServer queue
            , listenAgents connListen queue
            ]

connectServer :: (Functor m, MonadCatchIO m) => Address -> TQueue Event -> m ()
connectServer addr queue = connect addr $ do
    child <- forkContextFinally
       (forever $ liftIO (atomically $ readTQueue queue) >>= send)
       finish

    keepalive `finally` liftIO (killThread child)
  where
    keepalive = receive syn

    syn Syn = logPeerRX "SYN" >> send Ack >> logPeerTX "ACK" >> keepalive
    syn _   = logPeerRX "FIN"

listenAgents :: MonadCatchIO m => Address -> TQueue Event -> m ()
listenAgents addr queue = listen addr continue
  where
    continue = receive yield

    yield (E evt) = logPeerRX "EVT" >> liftIO (atomically $ writeTQueue queue evt) >> continue
    yield _       = logPeerRX "FIN"
