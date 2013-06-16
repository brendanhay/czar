{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main (main) where

import Control.Applicative
import Control.Concurrent
import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Czar.Agent.Config
import Data.Configurator
import Data.List                        (stripPrefix, isPrefixOf)
import Data.Monoid
import Network.BSD               hiding (hostName)
import Options
import System.Directory
import System.IO
import System.Log.Formatter
import System.Log.Handler               (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger
import System.ZMQ3.Monadic

import System.Locale (defaultTimeLocale)
import Data.Time (getCurrentTime, formatTime)

import qualified Control.Concurrent.Async as A
import qualified Control.Exception     as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T

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
    stringOption  "sendIpc" "socket" "ipc://czar.sock" "Path to the ipc socket."
    stringsOption "sendTags" "tags" [] "Comma separated list of tags to add."

defineOptions "ConnOpts" $ do
    stringOption  "connIpc" "socket" "ipc://czar.sock" "Path to the ipc socket."
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

        runZMQ $ localPush sendIpc "hello!"

        logInfo "Payload sent."

    conn_ opts = do
        ConnOpts{..} <- runScript $ validateConn opts
        logInfo $ "Identifying host as " ++ connHost
        logInfo "Starting agent ..."

        mapM_ (logInfo . ("Loading checks from " ++)) connChecks
        (cfg, tid) <- loadConfig connChecks

        checks <- mapM (parseCheck cfg) =<< getCheckNames cfg
        mapM_ (logInfo . T.unpack . (\c -> "Added " <> chkName c <> " check")) checks

        _ <- A.async . forever $ do
            threadDelay 1000000
            putStrLn "."

        asyncs <- zipWithM forkCheck checks . scanl1 (+) $ repeat connSplay
        mapM_ A.link asyncs

        chan   <- newChan

        E.finally
            (runZMQ $ do
                 localPull connIpc chan
                 remotePush connServer chan)
            (when `pathExistsM` connIpc $ removeFile (stripScheme connIpc))

localPush :: String -> BS.ByteString -> ZMQ z ()
localPush addr bs = do
    local <- pushSocket addr
    send local [] bs

localPull :: String -> Chan BS.ByteString -> ZMQ z ()
localPull addr chan = do
    local <- socket Pull
    bind local addr
    logInfo $ "Listening on " ++ addr
    async . forever $ do
        bs <- receive local
        logInfo $ "Received " ++ BS.unpack bs ++ " from " ++ addr
        liftIO $ writeChan chan bs

remotePush :: String -> Chan BS.ByteString -> ZMQ z ()
remotePush addr chan = do
    remote <- pushSocket addr
    forever $ do
        bs <- liftIO $ readChan chan
--        send remote [] bs
        logInfo $ "Sent " ++ BS.unpack bs ++ " to " ++ addr

pushSocket :: String -> ZMQ z (Socket z Push)
pushSocket addr = do
    sock <- socket Push
    connect sock addr
    logInfo $ "Connected to " ++ addr
    return sock

validateSend :: SendOpts -> Script SendOpts
validateSend opts@SendOpts{..} = do
    validateIpc sendIpc
    unless `pathExistsM` sendIpc $ do
        logError $ "Agent socket not found at " ++ sendIpc
        throwT "Agent not running."
    return opts

validateConn :: ConnOpts -> Script ConnOpts
validateConn opts@ConnOpts{..} = do
    validateIpc connIpc
    when `pathExistsM` connIpc $ do
        logError $ "Agent socket exists at " ++ connIpc
        throwT "Agent already running."
    name <- if null connHost
            then scriptIO getHostName
            else return connHost
    return $ opts { connHost = name }

validateIpc :: String -> Script ()
validateIpc str = unless ("ipc://" `isPrefixOf` str) $ do
    throwT $ str ++ " does not start with the required ipc:// scheme"

pathExistsM :: MonadIO m => (Bool -> a -> m ()) -> FilePath -> a -> m ()
pathExistsM cond path f =
    liftIO (doesFileExist $ stripScheme path) >>= flip cond f

stripScheme :: String -> String
stripScheme = T.unpack . last . T.splitOn "://" . T.pack

setLogging :: IO ()
setLogging = do
    removeAllHandlers
    hd <- streamHandler stderr INFO
    updateGlobalLogger logName (setLevel INFO . setHandlers [formatLog hd])

logName :: String
logName = "log"

logMsg :: MonadIO m => (String -> a -> IO ()) -> a -> m ()
logMsg f = liftIO . f logName

logInfo, logWarning, logError :: MonadIO m => String -> m ()
logInfo    = logMsg infoM
logWarning = logMsg warningM
logError   = logMsg errorM

formatLog :: GenericHandler Handle -> GenericHandler Handle
formatLog hd = setFormatter hd $ varFormatter [("nid", nid), ("utc", utc)] fmt
  where
    fmt = "[$utc $pid:$nid $prio] $msg"
    utc = formatTime defaultTimeLocale "%F %X %Z" <$> getCurrentTime
    nid = fromMaybe "0" . stripPrefix "ThreadId " . show <$> myThreadId
