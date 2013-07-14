-- |
-- Module      : Control.Concurrent.Race
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Control.Concurrent.Race where

import Control.Applicative
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import System.Posix.Signals

raceAll :: MonadIO m => [IO a] -> m a
raceAll xs = liftIO $ do
    ys <- mapM async xs

    void $ installHandler sigINT
        (CatchOnce $ mapM_ cancel ys)
        (Just emptySignalSet)

    snd <$> waitAnyCancel ys
