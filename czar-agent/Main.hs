{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main (main) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Data.List                        (stripPrefix)
import Data.Text                        (Text)
import Network.BSD               hiding (hostName)
import Options
import System.Directory
import System.FilePath
import System.IO
import System.Log.Formatter
import System.Log.Handler               (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger
import System.ShQQ
import System.ZMQ3.Monadic       hiding (async)


import System.Locale (defaultTimeLocale)
import Data.Time (getZonedTime,getCurrentTime,formatTime)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T
import qualified Data.Text.IO          as T

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
    stringOption  "sendIpc" "socket" "czar.sock" "Path to the ipc socket."
    stringsOption "sendTags" "tags" [] "Comma separated list of tags to add."

defineOptions "ConnOpts" $ do
    stringOption  "connIpc" "socket" "czar.sock" "Path to the ipc socket."
    stringOption  "connServer" "server" "" "Server for the agent to connect to."
--    stringOption "checks" "checks"   "checks"  "Directory containing a flat list of check descriptions in yaml."
    stringOption  "connHost" "hostname" "" "Hostname used to identify the machine."
    stringsOption "connTags" "tags" [] "Comma separated list of tags to always send."

main :: IO ()
main = runSubcommand
    [ cmd "send"    send_
    , cmd "connect" conn_
    ]
  where
    cmd name f = subcommand name $ \(_ :: MainOpts) x _ -> do
        setLogging
        runScript $ f x

    send_ opts = do
        validateSend opts
        return ()

    conn_ opts = do
        ConnOpts{..} <- validateConn opts
        logInfo $ "Identifying host as " ++ connHost

        logInfo "Starting agent ..."
        (push, rep) <- scriptIO $ do
            chan <- newChan
            (,) <$> async (pushSocket  connServer chan)
                <*> async (replySocket connIpc chan)

--        logInfo "Loading check configuration ..."
--        traverseFiles putStrLn (checks opts')

        logInfo "Waiting ..."
        scriptIO $ wait push

validateSend :: SendOpts -> Script ()
validateSend SendOpts{..} = do
    pathExistsM unless sendIpc $ do
        logError $ "Agent socket not found at " ++ sendIpc
        throwT "Agent not running."

validateConn :: ConnOpts -> Script ConnOpts
validateConn opts@ConnOpts{..} = do
    scriptIO $ doesFileExist connIpc >>= print
    pathExistsM when connIpc $ do
        logError $ "Agent socket exists at " ++ connIpc
        throwT "Agent already running."
    name <- if null connHost
            then scriptIO getHostName
            else return connHost
    return $ opts { connHost = name }

pathExistsM :: (Bool -> a -> Script ()) -> FilePath -> a -> Script ()
pathExistsM cond path f = scriptIO (doesFileExist $ strip path) >>= flip cond f
  where
    strip = T.unpack . last . T.splitOn "://" . T.pack

-- defaults :: AgentOpts -> IO AgentOpts
-- defaults opts              = return opts
-- defaults opts@ConnOpts{..} = do

-- check :: AgentOpts -> IO ()
-- check AgentOpts{..} = do
--     when (T.null tcpName)  $ throwT "Missing --server option."
--     when (T.null hostName) $ throwT "Missing --hostname option."

setLogging :: IO ()
setLogging = do
    removeAllHandlers
    hd <- streamHandler stderr INFO
    updateGlobalLogger logName (setLevel INFO . setHandlers [formatLog hd])

logName :: String
logName = "log"

logMsg :: (String -> a -> IO ()) -> a -> Script ()
logMsg f = scriptIO . f logName

logInfo, logWarning, logError :: String -> Script ()
logInfo    = logMsg infoM
logWarning = logMsg warningM
logError   = logMsg errorM

formatLog :: GenericHandler Handle -> GenericHandler Handle
formatLog hd = setFormatter hd $ varFormatter [("nid", nid), ("utc", utc)] fmt
  where
    fmt = "[$utc $pid:$nid $prio] $msg"
    utc = formatTime defaultTimeLocale "%F %X %Z" <$> getCurrentTime
    nid = fromMaybe "0" . stripPrefix "ThreadId " . show <$> myThreadId

pushSocket :: MonadIO m => String -> Chan BS.ByteString -> m ()
pushSocket addr chan = runZMQ $ do
    push <- socket Push
    connect push addr
    forever $ do
        item <- liftIO $ readChan chan
        send push [] item

replySocket :: MonadIO m => String -> Chan BS.ByteString -> m ()
replySocket addr chan = runZMQ $ do
    rep <- socket Rep
    bind rep addr
    forever $ do
        bs <- receive rep
        liftIO $ writeChan chan bs

traverseFiles :: (FilePath -> IO ()) -> FilePath -> IO ()
traverseFiles f = foldFiles (\_ n -> f n) ()

foldFiles :: (a -> FilePath -> IO a) -> a -> FilePath -> IO a
foldFiles f !a !d = do
    c <- check d
    case c of
        (True, True, False) -> f a d
        (True, False, True) -> foldDir
        _                   -> return a
  where
    foldDir = do
        xs <- map (d </>) . filter dots <$> getDirectoryContents d
        foldM (foldFiles f) a xs

    dots "."  = False
    dots ".." = False
    dots _    = True

    check x = (,,)
        <$> (readable <$> getPermissions x)
        <*> doesFileExist x
        <*> doesDirectoryExist x
