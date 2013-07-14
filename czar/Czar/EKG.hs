{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module      : Czar.EKG
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)


module Czar.EKG
    ( Stats

    , newStats
    , healthCheck

    -- * Re-exported
    , getCounter
    , getGauge
    , getLabel
    ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Czar.Internal.Protocol.Event     as E
import qualified Czar.Internal.Protocol.Metric    as M
import qualified Czar.Internal.Protocol.Threshold as T
import           Czar.Log
import           Czar.Protocol
import           Czar.Types
import qualified Data.HashMap.Strict              as Map
import           Data.IORef
import           Data.Maybe
import qualified Data.Sequence                    as Seq
import qualified Data.Text.Lazy                   as LT
import qualified Data.Text.Lazy.Encoding          as LT
import           Data.Time.Clock.POSIX
import qualified System.Remote.Common             as EKG
import           System.Remote.Common             hiding (Metric)
import           Text.ProtocolBuffers.Basic

-- | Type representing a czar component's internal metrics
data Stats = Stats
    { statsHost   :: !Utf8
    , statsKey    :: !Utf8
    , statsDesc   :: !(Maybe Utf8)
    , statsTags   :: !(Seq Utf8)
    , statsServer :: !Server
    }

newStats :: (Functor m, MonadIO m)
         => ByteString
         -> ByteString
         -> Maybe ByteString
         -> [ByteString]
         -> m Stats
newStats host key desc tags =
    Stats (Utf8 host) (Utf8 key) (Utf8 <$> desc) (toSeq tags) <$> server
  where
    server = liftIO $ Server
        <$> myThreadId
        <*> newIORef Map.empty
        <*> newIORef Map.empty
        <*> newIORef Map.empty

healthCheck :: (Functor m, MonadIO m)
            => Seconds
            -> Stats
            -> (E.Event -> m a)
            -> m a
healthCheck n stats action = forever $ do
    liftIO $ threadDelay delay
    logDebug "sampling internal stats"
    sampleStats stats >>= action
  where
    delay = toInt n

--
-- Internal
--

sampleStats :: MonadIO m => Stats -> m E.Event
sampleStats Stats{..} = liftIO $ do
    time <- truncate <$> getPOSIXTime
    newEvent time <$> sampleAll statsServer
  where
    newEvent time ms = E.Event
        { E.time        = time
        , E.host        = statsHost
        , E.key         = statsKey
        , E.description = statsDesc
        , E.tags        = statsTags
        , E.metrics     = Seq.fromList $ fromMap time ms
        , E.attributes  = Seq.empty
        }

    fromMap ts = mapMaybe (fromPair ts) . Map.toList

    fromPair ts (k, v) = newMetric ts (fromText k) v

    fromText = Utf8 . LT.encodeUtf8 . LT.fromStrict

newMetric :: Int64 -> Utf8 -> EKG.Metric -> Maybe M.Metric
newMetric time key metric = do
    (typ, val) <- case metric of
        (EKG.Counter n) -> Just ("c", n)
        (EKG.Gauge n)   -> Just ("g", n)
        _               -> Nothing
    return M.Metric
        { M.time     = time
        , M.type'    = Just typ
        , M.key      = key
        , M.value    = toDouble val
        , M.warning  = T.Threshold Nothing Nothing
        , M.critical = T.Threshold Nothing Nothing
        }
  where
    toDouble (I i) = fromIntegral i
    toDouble (D d) = d

toSeq :: [ByteString] -> Seq Utf8
toSeq = Seq.fromList . map Utf8

