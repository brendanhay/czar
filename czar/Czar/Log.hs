{-# LANGUAGE OverloadedStrings #-}

module Czar.Log (
      scriptLogging
    , logInfoM
    , logInfo
    , logWarn
    , logError
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Error
import Control.Monad.IO.Class
import Data.List                 (stripPrefix)
import Data.Time                 (getCurrentTime, formatTime)
import System.IO
import System.Locale             (defaultTimeLocale)
import System.Log.Formatter
import System.Log.Handler        (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger

scriptLogging :: Script a -> IO a
scriptLogging action = runScript $ setLogging >> action

logInfoM :: MonadIO m => (a -> String) -> [a] -> m ()
logInfoM = withPrefix logInfo

logInfo, logWarn, logError :: MonadIO m => String -> m ()
logInfo  = logMsg infoM
logWarn  = logMsg warningM
logError = logMsg errorM

--
-- Internal
--

setLogging :: MonadIO m => m ()
setLogging = liftIO $ do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    removeAllHandlers
    hd <- streamHandler stderr INFO
    updateGlobalLogger logName (setLevel INFO . setHandlers [formatLog hd])

logName :: String
logName = "log"

logMsg :: MonadIO m => (String -> a -> IO ()) -> a -> m ()
logMsg f = liftIO . f logName

withPrefix :: MonadIO m => (String -> m ()) -> (a -> String) -> [a] -> m ()
withPrefix f g = mapM_ (f . g)

formatLog :: GenericHandler Handle -> GenericHandler Handle
formatLog hd = setFormatter hd $ varFormatter [("nid", nid), ("utc", utc)] fmt
  where
    fmt = "[$utc $pid:$nid $prio] $msg"
    utc = formatTime defaultTimeLocale "%F %X %Z" <$> getCurrentTime
    nid = fromMaybe "0" . stripPrefix "ThreadId " . show <$> myThreadId
