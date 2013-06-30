-- |
-- Module      : Czar.EKG
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)


module Czar.EKG
    ( newStats
    , sampleStats

    -- * Re-exported
    , Server
    , getCounter
    , getGauge
    , getLabel
    ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Data.IORef
import           System.Remote.Common

import qualified Data.HashMap.Strict    as M

newStats :: MonadIO m => m Server
newStats = liftIO $ Server
    <$> myThreadId
    <*> newIORef M.empty
    <*> newIORef M.empty
    <*> newIORef M.empty

sampleStats :: MonadIO m => Server -> m Metrics
sampleStats = liftIO . sampleAll
