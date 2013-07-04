{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE ConstraintKinds                 #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE RecordWildCards                 #-}
{-# LANGUAGE ScopedTypeVariables             #-}

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

data Send = Send
    { sendAgent :: Address
    , sendTags  :: [String]
    , sendDebug :: Bool
    }

data Connect = Connect
    { connListen  :: Address
    , connServer  :: Address
    , connHost    :: String
    , connMetrics :: Seconds
    , connChecks  :: [FilePath]
    , connSplay   :: Seconds
    , connTags    :: [String]
    , connDebug   :: Bool
    }

data Command
    = S Send
    | C Connect

instance CommonOptions Command where
    debug (S s) = sendDebug s
    debug (C c) = connDebug c

sendCommand = command "send" . info (S <$> parser) $ progDesc ""
  where
    parser = Send
        <$> addressOption "agent" defaultAgent
                "Czar Agent address to connect to"

        <*> stringsOption "tags" []
                "Comma separated list to tags to annotate the event"

        <*> debugSwitch

connectCommand = command "connect" . info (C <$> parser) $ progDesc ""
  where
    parser = Connect
        <$> addressOption "listen" defaultAgent
                "Listen address for Czar Agent connections"

        <*> addressOption "server" defaultServer
                "Czar Agent address to connect to"

        <*> stringOption "host" ""
                "Hostname used to identify the machine (auto-determined if blank)"

        <*> secondsOption "metrics-interval" 30
                "Interval between internal metric emissions"

        <*> stringsOption "checks" ["etc/czar/checks.list.d"]
                "Comma separated list of files or directories containing check config"

        <*> secondsOption "splay" 1
                "Stagger check start times by this number of seconds"

        <*> stringsOption "tags" []
                "Comma separated list of tags to prepend to every event"

        <*> debugSwitch

program :: ParserInfo Command
program = info (helper <*> subparser (sendCommand <> connectCommand)) $
       fullDesc
    <> progDesc "asdsad"
    <> header "adsdas"

main :: IO ()
main = runProgram program $ \opts ->
    case opts of
        S s -> runSend s
        C c -> runConnect c
  where
    runSend Send{..} = do
        logInfo "connecting to agent ..."

        connect sendAgent $ do
            send $ E.Event 0 "hi!" "key" Nothing (Seq.fromList []) (Seq.fromList []) (Seq.fromList [])
            logInfo $ "payload sent to " ++ show sendAgent

        logInfo "Done."

    runConnect Connect{..} = do
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
            (fromString connHost)
            "czar.agent.internal"
            Nothing
            ["czar-agent"]

        raceAll
            [ healthCheck connMetrics stats (atomically . writeTQueue queue)
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
