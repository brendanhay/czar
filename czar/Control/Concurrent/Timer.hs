{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Control.Concurrent.MTimer
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Control.Concurrent.Timer where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.IORef

newtype Seconds = Seconds Int deriving (Eq, Ord, Num, Real)

data Timer = Timer !(IORef Bool) !ThreadId

start :: MonadIO m => Seconds -> IO () -> IO () -> m Timer
start (Seconds n) action cleanup = liftIO $ do
    ref <- newIORef False
    tid <- forkIO $ run ref
    return $! Timer ref tid
  where
    run ref = do
        p <- action >> threadDelay delay >> readIORef ref
        if p
         then run ref
         else cleanup

    delay = n * 1000000

reset :: MonadIO m => Timer -> m ()
reset (Timer ref _) = liftIO $ atomicWriteIORef ref True

cancel :: MonadIO m => Timer -> m ()
cancel (Timer _ tid) = liftIO $ killThread tid
