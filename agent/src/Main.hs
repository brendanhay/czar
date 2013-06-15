{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

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

defineOptions "AgentOpts" $ do
    stringOption  "tcpName"  "server"   ""  "Server for the agent to connect to."
    stringOption  "ipcName"  "socket"   "ipc://czar.sock"  "Path to the ipc socket."
    stringOption  "checks"   "checks"   "checks"  "Directory containing a flat list of check descriptions in yaml."
    stringOption  "hostName" "hostname" ""  "Hostname used to identify the machine."
    stringsOption "tags"     "tags"     []  "Comma separated list of of subscription tags."

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

main :: IO ()
main = runCommand $ \opts _ -> do
    setLogging

    opts' <- defaults opts
    logInfo $ "Identifying host as " ++ hostName opts'

    -- check opts'

    logInfo "Starting agent ..."
    chan <- newChan
    push <- async $ pushSocket  (tcpName opts') chan
    rep  <- async $ replySocket (ipcName opts') chan

    logInfo "Loading check configuration ..."
    traverseFiles putStrLn (checks opts')

    logInfo "Waiting ..."
    wait push

defaults :: AgentOpts -> IO AgentOpts
defaults opts@AgentOpts{..} = do
    name <- if null hostName then getHostName else return hostName
    return $ opts { hostName = name }

-- check :: AgentOpts -> IO ()
-- check AgentOpts{..} = do
--     when (T.null tcpName)  $ throwT "Missing --server option."
--     when (T.null hostName) $ throwT "Missing --hostname option."

setLogging = do
    removeAllHandlers
    hd <- streamHandler stderr INFO
    updateGlobalLogger logName (setLevel INFO . setHandlers [formatLog hd])

logName = "log"

logInfo    = infoM logName
logWarning = warningM logName
logError   = errorM logName

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
