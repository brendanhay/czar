-- |
-- Module      : Control.Concurrent.Race
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Control.Concurrent.Race where

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import System.Posix.Signals

linkedRace_ :: MonadIO m => IO a -> IO b -> m ()
linkedRace_ left right = linkedRace left right >> return ()

linkedRace :: MonadIO m => IO a -> IO b -> m (Either a b)
linkedRace left right = liftIO $ do
    a <- async left
    b <- async right

    link2 a b

    void $ installHandler sigINT
        (CatchOnce $ cancel a >> cancel b)
        (Just emptySignalSet)

    waitEither a b
