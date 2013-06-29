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

import Control.Concurrent.Race
import Control.Concurrent.STM
import Control.Error
import Control.Monad
import Control.Monad.CatchIO
import Control.Monad.IO.Class
import Network.BSD              hiding (hostName)
import Network.Socket                  (SockAddr)
import Options

import Czar.Agent.Check
import Czar.Log
import Czar.Protocol
import Czar.Socket

import qualified Data.Sequence as Seq
import qualified Data.Text     as T

import qualified Czar.Internal.Protocol.Event as E

defineOptions "MainOpts" $ return ()

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
        \(_ :: MainOpts) opts _ -> scriptLogging $ action opts

runSend :: SendOpts -> Script ()
runSend SendOpts{..} = do
    addr <- parseAddr sendAgent

    logInfo "Connecting to agent ..."

    scriptIO . connect addr $ do
        send $ E.Event 0 "hi!" "key" Nothing (Seq.fromList []) (Seq.fromList []) (Seq.fromList [])
        logInfo $ "Payload sent to " ++ show addr

    logInfo "Done."

runConnect :: ConnOpts -> Script ()
runConnect ConnOpts{..} = do
    host <- if null connHost
            then liftIO getHostName
            else return connHost

    srv  <- parseAddr connServer
    lst  <- parseAddr connListen

    logInfo $ "Identifying host as " ++ host
    logInfo "Starting agent ..."

    scriptIO $ do
        logInfoM ("Loading checks from " ++) connChecks

        checks <- loadChecks connChecks
        queue  <- atomically newTQueue

        logInfoM (("Adding " ++) . T.unpack . chkName) checks

        forkChecks connSplay queue checks

        linkedRace_
            (connectServer srv queue)
            (listenAgents lst queue)

connectServer :: MonadCatchIO m => SockAddr -> TQueue Event -> m ()
connectServer addr queue = connect addr $ do
    peer <- peerName
    _    <- fork $ heartbeats peer

    forever $ do
        evt <- liftIO . atomically $ readTQueue queue
        send evt
        logInfo $ "Sent " ++ show evt ++ " to " ++ show addr
  where
    heartbeats = receive . handle

    handle p Syn = logInfo ("SYN <- " ++ p) >> send Ack >> logInfo ("ACK ->" ++ p) >> heartbeats p
    handle p _   = logWarn ("UNKNOWN/FIN -> " ++ p) >> return ()

listenAgents :: MonadCatchIO m => SockAddr -> TQueue Event -> m ()
listenAgents addr queue = listen addr $ do
    logInfo "Accepted agent connection"
    loop
  where
    loop = receive handle

    handle (E evt) = liftIO (atomically $ writeTQueue queue evt) >> loop
    handle _       = return ()
