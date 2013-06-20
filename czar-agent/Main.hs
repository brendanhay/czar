{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main (main) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Czar.Agent.Config
import Czar.Log
import Czar.Socket
import Network.BSD              hiding (hostName)
import Options
import System.ZMQ3.Monadic      hiding (async)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T

defineOptions "MainOpts" $ return ()

defineOptions "SendOpts" $ do
    stringOption  "sendAgent" "agent" "ipc://czar-agent.sock" "Path to the ipc socket."
    stringsOption "sendTags"   "tags"  [] "Comma separated list of tags to add."

defineOptions "ConnOpts" $ do
    stringOption  "connListen" "listen"   "ipc://czar-agent.sock" "Path to the ipc socket."
    stringOption  "connPublish" "server"   "tcp://localhost:5555" "Server for the agent to connect to."
    stringOption  "connHost"   "hostname" "" "Hostname used to identify the machine."
    integerOption "connSplay"  "splay"    50 "Given a list of checks, differentiate their start times by this number of milliseconds."
    stringsOption "connTags"   "tags"     [] "Comma separated list of tags to always send."
    stringsOption "connChecks" "checks"   ["etc/czar/checks.list.d"] "Paths to files or directories containing check configuration."

main :: IO ()
main = runSubcommand
    [ cmd "send"    send_
    , cmd "connect" connect_
    ]
  where
    cmd name f = subcommand name $
        \(_ :: MainOpts) x _ -> runScript $ setLogging >> f x

send_ :: SendOpts -> Script ()
send_ SendOpts{..} = do
    uri <- parseUri sendAgent

    logInfo "Connecting to agent ..."

    scriptIO $ runZMQ $ do
        sock <- agentNotify uri
        send sock [] "Hello!"
        logInfo $ "Payload sent to " ++ show uri

    logInfo "OK"

connect_ :: ConnOpts -> Script ()
connect_ ConnOpts{..} = do
    host <- if null connHost then liftIO getHostName else return connHost

    luri <- parseUri connListen
    puri <- parseUri connPublish

    logInfo $ "Identifying host as " ++ host
    logInfo "Starting agent ..."

    logInfoM ("Loading checks from " ++) connChecks

    config <- liftIO $ loadConfig connChecks
    checks <- liftIO $ parseChecks config
    chan   <- liftIO $ atomically newTChan

    logInfoM (("Adding " ++) . T.unpack . chkName) checks

    liftIO $ forkChecks connSplay chan checks

    tryCatchIO (cleanup luri) $ do
        l <- listen luri chan
        r <- publish puri chan
        waitEither_ l r
  where
    listen uri chan = async $ runZMQ $ do
        sock <- agentListen uri
        forever $ do
            bs <- receive sock
            logInfo $ "Received " ++ BS.unpack bs ++ " on " ++ show uri
            liftIO . atomically $ writeTChan chan bs

    publish uri chan = async $ runZMQ $ do
        sock <- agentPublish uri
        forever $ do
            bs <- liftIO . atomically $ readTChan chan
            send sock [] bs
            logInfo $ "Sent " ++ BS.unpack bs ++ " to " ++ show uri
