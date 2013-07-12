{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : Health
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import           Control.Applicative
import           Control.Error
import           Control.Monad.IO.Class
import qualified Czar.Internal.Protocol.Metric    as M
import           Czar.Internal.Protocol.Threshold
import           Czar.Options
import           Czar.Protocol
import           Data.Time.Clock.POSIX
import           Network.BSD
import           System.Info.Health

commonOptions "Check" $ do
    maybeStringOption "chkHost" "host"
        "Host of the event"

    stringOption "chkKey" "key" "cpu"
        "Event key"

    stringOption "chkDesc" "description" "CPU Utilisation"
        "Description of the event"

    stringsOption "chkTags" "tags" ["cpu", "host"]
        "Tags to associate with the event"

    thresholdOption "warnLoad" "load-warning" (Threshold Nothing (Just 0.8))
        "Thresholds for load warnings"

    thresholdOption "critLoad" "load-critical" (Threshold Nothing (Just 0.95))
        "Thresholds for load criticals"

    thresholdOption "warnCpu" "cpu-warning" (Threshold Nothing (Just 0.8))
        "Thresholds for cpu warnings"

    thresholdOption "critCpu" "cpu-critical" (Threshold Nothing (Just 0.95))
        "Thresholds for cpu criticals"

    thresholdOption "warnMem" "mem-warning" (Threshold Nothing (Just 0.75))
        "Thresholds for memory warnings"

    thresholdOption "critMem" "mem-critical" (Threshold Nothing (Just 0.85))
        "Thresholds for memory criticals"

main :: IO ()
main = runProgram $ \Check{..} -> do
    Health{..} <- runScript getHealth

    host <- maybe (liftIO getHostName) return chkHost
    time <- truncate <$> getPOSIXTime

    let evt = createEvent time host chkKey
                  (if null chkDesc then Nothing else Just chkDesc)
                  chkTags
                  [ M.Metric time Nothing "load" healthLoad warnLoad critLoad
                  , M.Metric time Nothing "cpu" healthUtil warnCpu critCpu
                  , M.Metric time Nothing "memory" healthMemory warnMem critMem
                  ]

    print evt
