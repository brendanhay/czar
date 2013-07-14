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
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.CatchIO               hiding (Handler)
import           Control.Monad.IO.Class
import           Czar.EKG
import qualified Czar.Internal.Protocol.Event        as E
import           Czar.Internal.Protocol.Subscription
import           Czar.Log
import           Czar.Options
import           Czar.Protocol
import           Czar.Socket
import qualified Data.Sequence                       as Seq

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

    connect hdServer $ do
        logPeerTX $ "sending subscription for " ++ show hdTags
        subscribe hdTags

        queue  <- liftIO $ atomically newTQueue
        stats  <- newStats
            "localhost"
            "czar.graphite"
            Nothing
            ["czar-graphite"]

        health <- liftIO . forkIO $ healthCheck optEmission stats
            (atomically . writeTQueue queue)

        sender <- forkContextFinally
            (forever $ liftIO (atomically $ readTQueue queue) >>= send)
            finish

        continue `finally` liftIO (killThread health >> killThread sender)
  where
    subscribe = send
        . Subscription "graphite" (Just "Graphite Handler")
        . Seq.fromList
        . map fromString

    continue = receive yield

    yield (E evt) = logPeerRX (show $ E.tags evt) >> continue
    yield Syn     = logPeerRX "SYN" >> send Ack >> logPeerTX "ACK" >> continue
    yield _       = logPeerRX "FIN"
