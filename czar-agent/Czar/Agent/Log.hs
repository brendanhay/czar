{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Czar.Agent.Log (
      logInfo
    , logWarning
    , logError
    , setLogging
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
import Text.Printf

setLogging :: IO ()
setLogging = do
    removeAllHandlers
    hd <- streamHandler stderr INFO
    updateGlobalLogger logName (setLevel INFO . setHandlers [formatLog hd])

-- logInfo, logWarning, logError :: MonadIO m => String -> String -> m ()
logInfo    = logMsg infoM . printf
logWarning = logMsg warningM . printf
logError   = logMsg errorM . printf

logName :: String
logName = "log"

logMsg :: MonadIO m => (String -> a -> IO ()) -> a -> m ()
logMsg f = liftIO . f logName

formatLog :: GenericHandler Handle -> GenericHandler Handle
formatLog hd = setFormatter hd $ varFormatter [("nid", nid), ("utc", utc)] fmt
  where
    fmt = "[$utc $pid:$nid $prio] $msg"
    utc = formatTime defaultTimeLocale "%F %X %Z" <$> getCurrentTime
    nid = fromMaybe "0" . stripPrefix "ThreadId " . show <$> myThreadId
