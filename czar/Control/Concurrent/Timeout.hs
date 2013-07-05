-- |
-- Module      : Control.Concurrent.Timeout
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Control.Concurrent.Timeout
    ( Timeout
    , start
    , reset
    , cancel
    ) where

import Control.Concurrent
import Control.Monad.IO.Class
import Czar.Types
import Data.IORef

data Cont = Reset | Action | Stop

data Timeout = Timeout !(IORef Cont) !ThreadId

start :: MonadIO m => Seconds -> IO () -> IO () -> m Timeout
start n action cleanup = liftIO $ do
    ref <- newIORef Action
    tid <- forkIO $ run ref
    return $! Timeout ref tid
  where
    run = (threadDelay delay >>) . continue

    continue ref = do
        val <- readIORef ref
        case val of
            Reset  -> atomicWriteIORef ref Action >> run ref
            Action -> action >> atomicWriteIORef ref Stop >> run ref
            Stop   -> cleanup

    delay = toInt n

reset :: MonadIO m => Timeout -> m ()
reset (Timeout ref _) = liftIO $ atomicWriteIORef ref Reset

cancel :: MonadIO m => Timeout -> m ()
cancel (Timeout _ tid) = liftIO $ killThread tid
