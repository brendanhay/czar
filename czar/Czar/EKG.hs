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
    ( startEKG
    , sampleAll

    -- * Re-exported
    , Server
    , getCounter
    , getGauge
    , getLabel
    ) where

import           Control.Monad.IO.Class
import           System.Remote.Common   hiding (sampleAll)

import qualified System.Remote.Common   as C

startEKG :: MonadIO m => m Server
startEKG = liftIO $ Server undefined
    <$> newIORef M.empty
    <*> newIORef M.empty
    <*> newIORef M.empty

sampleAll :: MonadIO m => Server -> m Metrics
sampleAll = liftIO . C.sampleAll
