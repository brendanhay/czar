{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module      : Czar.Threshold
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Czar.Threshold where

import Czar.Internal.Protocol.Event
import Czar.Internal.Protocol.Metric
import Czar.Internal.Protocol.Threshold
import Czar.Protocol                    ()
import Data.Foldable
import Data.Sequence                    ((<|))
import Prelude                          hiding (any)

data Level = Healthy | Warning | Critical
    deriving (Eq)

annotate :: Event -> Event
annotate evt = case classify evt of
    Critical -> add "critical"
    Warning  -> add "warning"
    Healthy  -> evt
  where
    add name = evt { tags = name <| (tags evt) }

classify :: Event -> Level
classify Event{..}
    | any (== Critical) levels = Critical
    | any (== Warning) levels  = Warning
    | otherwise               = Healthy
  where
    levels = measure `fmap` metrics

measure :: Metric -> Level
measure Metric{..}
    | value `exceeds` critical = Critical
    | value `exceeds` warning  = Warning
    | otherwise                = Healthy
  where
    exceeds n Threshold{..} = check (<=) lower && check (>=) upper
      where
        check op = maybe False (op n)
