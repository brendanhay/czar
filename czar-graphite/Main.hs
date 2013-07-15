{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import           Control.Concurrent                  hiding (yield)
import           Control.Concurrent.Race
import           Control.Concurrent.STM
import           Control.Monad                       (forever)
import           Control.Monad.CatchIO               hiding (Handler)
import           Control.Monad.IO.Class
import           Czar.EKG
import qualified Czar.Internal.Protocol.Event        as E
import qualified Czar.Internal.Protocol.Metric       as M
import           Czar.Internal.Protocol.Subscription
import           Czar.Log
import           Czar.Options
import           Czar.Protocol
import           Czar.Socket
import           Czar.Types
import qualified Data.ByteString.Char8               as BS
import           Data.Foldable
import           Data.Monoid
import qualified Data.Sequence                       as Seq
import           Prelude                             hiding (mapM_)
import           Text.Printf

commonOptions "Handler" $ do
    addressOption "hdServer" "server" defaultHandler
        "Czar Server address to connect to"

    addressOption "hdGraphite" "graphite" "unix://graphite.sock"
        "Graphite address to write metrics to"

    stringsOption "hdTags" "tags" ["*"]
        "Tags to subscribe to"

    secondsOption "optEmission" "metric-frequency" 30
        "Frequency of internal metric emissions"

main :: IO ()
main = runProgram $ \Handler{..} -> do
    logInfo "starting graphite handler ..."

    queue <- atomically newTQueue
    stats <- newStats
        "localhost"
        "czar.graphite"
        Nothing
        ["czar-graphite"]

    raceAll
        [ healthCheck optEmission stats (atomically . writeTQueue queue)
        , connectServer hdServer hdTags queue
        , connectGraphite hdGraphite queue
        ]

connectServer :: (Functor m, MonadCatchIO m)
              => Address
              -> [String]
              -> TQueue Event
              -> m ()
connectServer addr tags queue = connect addr $ do
    logPeerTX $ "sending subscription for " ++ show tags

    send . Subscription "graphite" (Just "Graphite Handler")
         . Seq.fromList
         $ map fromString tags

    child <- forkContextFinally
        (forever $ liftIO (atomically $ readTQueue queue) >>= send)
        finish

    continue `finally` liftIO (killThread child)
  where
    continue = receive yield

    yield (E evt) = do
        logPeerRX (show $ E.tags evt)
        liftIO . atomically $ writeTQueue queue evt
        continue

    yield Syn = logPeerRX "SYN" >> send Ack >> logPeerTX "ACK" >> continue
    yield _   = logPeerRX "FIN"

connectGraphite :: MonadCatchIO m
                => Address
                -> TQueue Event
                -> m ()
connectGraphite _addr queue = liftIO . forever $ do
    E.Event{..} <- atomically (readTQueue queue)
    mapM_ (print . line host key) metrics
  where
    line host pre M.Metric{..} = k <> " " <> v <> " " <> t
      where
        k = BS.intercalate "." $ map utf8ToBS [host, pre, key]
        v = BS.pack $ printf "%.8f" value
        t = BS.pack $ show time

        -- FIXME: Take type into account
        -- FIXME: Safely escape full keys
