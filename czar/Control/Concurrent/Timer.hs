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

data Timer = Timer !(IORef Bool) !ThreadId

start :: MonadIO m => Int -> IO () -> IO () -> m Timer
start n action cleanup = liftIO $ do
    ref <- newIORef False
    tid <- forkIO $ threadDelay n >> run ref
    return $! Timer ref tid
  where
    run ref = do
        p <- action >> threadDelay n >> readIORef ref
        if p
         then run ref
         else cleanup

reset :: MonadIO m => Timer -> m ()
reset (Timer ref _) = liftIO $ atomicWriteIORef ref True

cancel :: MonadIO m => Timer -> m ()
cancel (Timer _ tid) = liftIO $ killThread tid
