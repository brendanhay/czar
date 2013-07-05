{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

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

import           Control.Concurrent           hiding (yield)
import           Control.Concurrent.Race
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.IO.Class
import           Czar.Agent.Check
import           Czar.EKG
import qualified Czar.Internal.Protocol.Event as E
import           Czar.Log
import           Czar.Options
import           Czar.Protocol                hiding (S, fromString)
import           Czar.Socket
import           Czar.Types
import qualified Data.Sequence                as Seq
import           Data.String
import qualified Data.Text                    as T
import           Network.BSD                  hiding (hostName)

commonOptions "Main" $ return ()

defineOptions "Send" $ do
    addressOption "sendAgent" "agent" defaultAgent
        "Czar Agent address to connect to"

    stringsOption "sendTags" "tags" []
        "Comma separated list to tags to annotate the event"

defineOptions "Connect" $ do
    addressOption "connListen" "listen" defaultAgent
        "Listen address for Czar Agent connections"

    addressOption "connServer" "server" defaultServer
        "Czar Server address to connect to"

    stringOption "connHost" "host" ""
        "Hostname used to identify the machine (auto-determined if blank)"

    secondsOption "connMetrics" "metrics-interval" 30
        "Interval between internal metric emissions"

    stringsOption "connChecks" "checks" ["etc/czar/checks.list.d"]
        "Comma separated list of files or directories containing check config"

    secondsOption "connSplay" "splay" 1
        "Stagger check start times by this number of seconds"

    stringsOption "connTags" "tags" []
        "Comma separated list of tags to prepend to every event"

    secondsOption "optEmission" "metric-frequency" 30
        "Frequency of internal metric emissions"

main :: IO ()
main = runSubcommand
    [ subcommand "send" (run send')
    , subcommand "connect" (run connect')
    ]
  where
    run f Main{..} opts _ = f opts

    send' Send{..} = do
        logInfo "connecting to agent ..."

        connect sendAgent $ do
            send $ E.Event 0 "hi!" "key" Nothing (Seq.fromList []) (Seq.fromList []) (Seq.fromList [])
            logInfo $ "payload sent to " ++ show sendAgent

        logInfo "Done."

    connect' Connect{..} = do
        host <- if null connHost
                then liftIO getHostName
                else return connHost

        logInfo $ "identifying host as " ++ host
        logInfo "starting agent ..."

        logInfoM ("loading checks from " ++) connChecks

        checks <- loadChecks connChecks
        queue  <- atomically newTQueue

        logInfoM (("adding " ++) . T.unpack . chkName) checks
        forkChecks connSplay queue checks

        stats  <- newStats
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
