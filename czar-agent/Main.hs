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
--import Control.Concurrent.Async
import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Data.List                        (stripPrefix, isPrefixOf)
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
import System.ZMQ3.Monadic

import System.Locale (defaultTimeLocale)
import Data.Time (getZonedTime,getCurrentTime,formatTime)

import qualified Control.Exception     as E
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
    stringOption  "sendIpc" "socket" "ipc://czar.sock" "Path to the ipc socket."
    stringsOption "sendTags" "tags" [] "Comma separated list of tags to add."

defineOptions "ConnOpts" $ do
    stringOption  "connIpc" "socket" "ipc://czar.sock" "Path to the ipc socket."
    stringOption  "connServer" "server" "" "Server for the agent to connect to."
    stringOption  "connChecks" "checks" "checks" "Directory containing a flat list of check descriptions in yaml."
    stringOption  "connHost" "hostname" "" "Hostname used to identify the machine."
    stringsOption "connTags" "tags" [] "Comma separated list of tags to always send."

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
        logInfo "Loading check configuration ..."
        traverseFiles logInfo connChecks
        chan <- newChan
        E.finally
            (runZMQ $ localPull connIpc chan >> remotePush connServer chan)
            (when `pathExistsM` connIpc $ removeFile (stripScheme connIpc))

localPush addr bs = do
    local <- pushSocket addr
    send local [] bs

localPull addr chan = do
    local <- socket Pull
    bind local addr
    logInfo $ "Listening on " ++ addr
    async . forever $ do
        bs <- receive local
        logInfo $ "Received " ++ BS.unpack bs ++ " from " ++ addr
        liftIO $ writeChan chan bs

remotePush addr chan = do
    remote <- pushSocket addr
    forever $ do
        bs <- liftIO $ readChan chan
--        send remote [] bs
        logInfo $ "Sent " ++ BS.unpack bs ++ " to " ++ addr

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

-- -- pushSocket :: MonadIO m => String -> Chan BS.ByteString -> m ()
-- pushSocket addr chan = do
--     push <- socket Push
--     liftIO $ putStrLn $ "connecting to " ++ addr
--     connect push addr
--     liftIO $ putStrLn "connected"
--     forever $ do
--         item <- liftIO $ readChan chan
--         liftIO $ print item
--         send push [] item

replySocket :: MonadIO m => String -> Chan BS.ByteString -> m ()
replySocket addr chan = runZMQ $ do
    rep <- socket Rep
    bind rep addr
    forever $ do
        bs <- receive rep
        liftIO $ writeChan chan bs

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
