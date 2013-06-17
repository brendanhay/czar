{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main (main) where

import Control.Error
import Czar.Agent.Config
import Czar.Agent.Log
import Czar.Agent.Socket
import Data.Monoid
import Network.BSD         hiding (hostName)
import Options
import System.ZMQ3.Monadic

import qualified Data.Text as T

-- Remote execution agent
-- Push only initially

  -- Configuration is a flat-list of yaml files in a directory
  -- Each check gets it's own thread

-- Push socket to server

-- Local ipc socket
-- remove socket file on exit if this process was the creator

-- 2 mode agent, one is connect, other is push
-- use ipc socket to connect to running agent and specify
-- payload via command line flags

-- Use case:
-- chef run fails, invoke agent


-- TODO
-- agent
--   multimode connect
--     reads from ipc
--     writes to push
--   multimode push
--     writes to ipc
-- server
--   reads from pull + prints

defineOptions "MainOpts" $ return ()

defineOptions "SendOpts" $ do
    stringOption  "sendSocket" "socket" "ipc://czar.sock" "Path to the ipc socket."
    stringsOption "sendTags" "tags" [] "Comma separated list of tags to add."

defineOptions "ConnOpts" $ do
    stringOption  "connSocket" "socket" "ipc://czar.sock" "Path to the ipc socket."
    stringOption  "connServer" "server" "" "Server for the agent to connect to."
    stringOption  "connHost" "hostname" "" "Hostname used to identify the machine."
    integerOption "connSplay" "splay" 50 "Given a list of checks, differentiate their start times by this number of milliseconds."
    stringsOption "connTags" "tags" [] "Comma separated list of tags to always send."
    stringsOption "connChecks" "checks" ["etc/czar/checks.list.d"] "Paths to files or directories containing check configuration."

main :: IO ()
main = runSubcommand
    [ cmd "send"    send_
    , cmd "connect" conn_
    ]
  where
    cmd name f = subcommand name $ \(_ :: MainOpts) x _ -> setLogging >> f x

    send_ opts = do
        SendOpts{..} <- runScript $ validateSend opts

        logInfo "Connecting to agent ..."

        runZMQ $ writeSocket sendSocket "hello!"

        logInfo "Payload sent."

    conn_ opts = do
        ConnOpts{..} <- runScript $ validateConn opts
        logInfo $ "Identifying host as " ++ connHost
        logInfo "Starting agent ..."

        mapM_ (logInfo . ("Loading checks from " ++)) connChecks
        cfg <- loadConfig connChecks

        checks <- parseChecks cfg
        mapM_ (logInfo . T.unpack . (\c -> "Added " <> chkName c <> " check")) checks
        forkChecks connSplay checks

        pairSockets connSocket connServer id

validateSend :: SendOpts -> Script SendOpts
validateSend opts@SendOpts{..} = do
    validateSocket sendSocket
    unlessSocket sendSocket $ do
        logError $ "Agent socket not found at " ++ sendSocket
        throwT "Agent not running."
    return opts

validateConn :: ConnOpts -> Script ConnOpts
validateConn opts@ConnOpts{..} = do
    validateSocket connSocket
    whenSocket connSocket $ do
        logError $ "Agent socket exists at " ++ connSocket
        throwT "Agent already running."
    name <- if null connHost
            then scriptIO getHostName
            else return connHost
    return $ opts { connHost = name }
