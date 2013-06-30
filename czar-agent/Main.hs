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

import           Control.Concurrent.Race
import           Control.Concurrent.STM
import           Control.Error
import           Control.Monad.CatchIO
import           Control.Monad.IO.Class
import           Network.BSD                  hiding (hostName)
import           Network.Socket               (SockAddr)
import           Options

import qualified Data.Sequence                as Seq
import qualified Data.Text                    as T

import           Czar.Agent.Check
import           Czar.Log
import           Czar.Protocol
import           Czar.Socket

import qualified Czar.Internal.Protocol.Event as E

defineOptions "MainOpts" $
    boolOption "mainVerbose" "verbose" False
        "Be really loud."

defineOptions "SendOpts" $ do
    stringOption "sendAgent" "connect" defaultAgent
        "Socket of the agent to send to."

    stringsOption "sendTags" "tags" []
        "Comma separated list of tags to add."

defineOptions "ConnOpts" $ do
    stringOption "connListen" "listen" defaultAgent
        "Socket to listen on for incoming connections."

    stringOption "connServer" "server" defaultServer
        "Server for the agent to connect to."

    stringOption "connHost" "hostname" ""
        "Hostname used to identify the machine."

    integerOption "connSplay" "splay" 50
        "Given a list of checks, differentiate their start times by this number of milliseconds."

    stringsOption "connTags" "tags" []
        "Comma separated list of tags to always send."

    stringsOption "connChecks" "checks" ["etc/czar/checks.list.d"]
        "Paths to files or directories containing check configuration."

main :: IO ()
main = runSubcommand
    [ cmd "send"    runSend
    , cmd "connect" runConnect
    ]
  where
    cmd name action = subcommand name $
        \MainOpts{..} opts _ -> scriptLogging mainVerbose $ action opts

runSend :: SendOpts -> Script ()
runSend SendOpts{..} = do
    addr <- parseAddr sendAgent

    logInfo "connecting to agent ..."

    scriptIO . connect addr $ do
        send $ E.Event 0 "hi!" "key" Nothing (Seq.fromList []) (Seq.fromList []) (Seq.fromList [])
        logInfo $ "payload sent to " ++ show addr

    logInfo "Done."

runConnect :: ConnOpts -> Script ()
runConnect ConnOpts{..} = do
    host <- if null connHost
            then liftIO getHostName
            else return connHost

    srv  <- parseAddr connServer
    lst  <- parseAddr connListen

    logInfo $ "identifying host as " ++ host
    logInfo "starting agent ..."

    scriptIO $ do
        logInfoM ("loading checks from " ++) connChecks

        checks <- loadChecks connChecks
        queue  <- atomically newTQueue

        logInfoM (("adding " ++) . T.unpack . chkName) checks

        forkChecks connSplay queue checks

        linkedRace_
            (connectServer srv queue)
            (listenAgents lst queue)

connectServer :: (Functor m, MonadCatchIO m) => SockAddr -> TQueue Event -> m ()
connectServer addr queue = connect addr $ do
    fork $ do
        evt <- liftIO . atomically $ readTQueue queue
        send evt

    keepalive
  where
    keepalive = receive heartbeat

    heartbeat Syn = logPeerRX "SYN" >> send Ack >> logPeerTX "ACK" >> keepalive
    heartbeat _   = logPeerRX "FIN"

listenAgents :: MonadCatchIO m => SockAddr -> TQueue Event -> m ()
listenAgents addr queue = listen addr continue
  where
    continue = receive yield

    yield (E evt) = logPeerRX "EVT" >> liftIO (atomically $ writeTQueue queue evt) >> continue
    yield _       = logPeerRX "FIN"
