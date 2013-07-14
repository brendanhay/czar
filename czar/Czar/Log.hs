-- |
-- Module      : Czar.Log
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Czar.Log
    ( setLogging
    , logM
    , logInfoM
    , logInfo
    , logWarn
    , logError
    , logDebug

    -- * Re-exported Types
    , Priority(..)
    ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Data.Time                 (getCurrentTime, formatTime)
import           System.IO
import           System.Locale             (defaultTimeLocale)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple
import qualified System.Log.Logger         as L
import           System.Log.Logger         hiding (logM)
import           System.Posix.Process      (getProcessID)
import           Text.Printf

setLogging :: MonadIO m => Bool -> m ()
setLogging debug = liftIO $ do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    removeAllHandlers
    hd <- streamHandler stderr prio
    updateGlobalLogger logName (setLevel prio . setHandlers [formatLog hd])
  where
    prio = if debug then DEBUG else INFO

logM :: MonadIO m => Priority -> String -> m ()
logM prio = liftIO . L.logM logName prio

logInfoM :: MonadIO m => (a -> String) -> [a] -> m ()
logInfoM = withPrefix logInfo

logInfo, logWarn, logError, logDebug :: MonadIO m => String -> m ()
logInfo  = logMsg infoM
logWarn  = logMsg warningM
logError = logMsg errorM
logDebug = logMsg debugM

--
-- Internal
--

logName :: String
logName = "log"

logMsg :: MonadIO m => (String -> a -> IO ()) -> a -> m ()
logMsg f = liftIO . f logName

withPrefix :: MonadIO m => (String -> m ()) -> (a -> String) -> [a] -> m ()
withPrefix f g = mapM_ (f . g)

formatLog :: GenericHandler Handle -> GenericHandler Handle
formatLog hd = setFormatter hd fmt
  where
    fmt _ (prio, msg) _ = do
        tid <- drop 9 . show <$> myThreadId
        pid <- show <$> getProcessID
        utc <- ts <$> getCurrentTime

        return $ printf "%s %s{%02s} %-7s %s" utc pid tid (show prio ++ ":") msg

    ts = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
