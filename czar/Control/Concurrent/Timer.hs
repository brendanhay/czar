{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Control.Concurrent.Timer
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Control.Concurrent.Timer (
      Seconds
    , Timer

    , threadSleep
    , startTimer
    , resetTimer
    , cancelTimer
    ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class

newtype Seconds = Seconds Int deriving (Eq, Ord, Num, Real)

data Timer = Timer (MVar Ref)

data Ref = Ref Seconds (IO ()) (Async ())

threadSleep :: MonadIO m => Seconds -> m ()
threadSleep (Seconds n) = liftIO . threadDelay $ n * 1000000

startTimer :: (Functor m, MonadIO m) => Seconds -> IO () -> m Timer
startTimer n action = createRef n action >>= fmap Timer . liftIO . newMVar

resetTimer :: MonadIO m => Timer -> m ()
resetTimer (Timer var) = liftIO $ modifyMVar_ var reset
  where
    reset (Ref n action ref) = cancel ref >> createRef n action

cancelTimer :: MonadIO m => Timer -> m ()
cancelTimer (Timer var) = liftIO $ do
    (Ref _ _ ref) <- readMVar var
    cancel ref

--
-- Internal
--

createRef :: MonadIO m => Seconds -> IO () -> m Ref
createRef n action = liftIO $ do
    ref <- async $ threadSleep n >> action
    return $! Ref n action ref

