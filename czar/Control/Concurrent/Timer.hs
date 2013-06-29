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
    ) where

import Control.Concurrent
import Control.Concurrent.Async

newtype Seconds = Seconds Int deriving (Eq, Ord, Num, Real)

data Timer = Timer (MVar Ref)

data Ref = Ref Seconds (IO ()) (Async ())

threadSleep :: Seconds -> IO ()
threadSleep (Seconds n) = threadDelay $ n * 1000000

startTimer :: Seconds -> IO () -> IO Timer
startTimer n action = createRef n action >>= fmap Timer . newMVar

resetTimer :: Timer -> IO ()
resetTimer (Timer var) = modifyMVar_ var reset
  where
    reset (Ref n action ref) = cancel ref >> createRef n action

--
-- Internal
--

createRef :: Seconds -> IO () -> IO Ref
createRef n action = do
    ref <- async $ threadSleep n >> action
    link ref
    return $! Ref n action ref

