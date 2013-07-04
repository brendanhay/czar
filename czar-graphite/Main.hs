{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import qualified Czar.Internal.Protocol.Event        as E
import           Czar.Internal.Protocol.Subscription
import           Czar.Log
import           Czar.Options
import           Czar.Protocol
import           Czar.Socket
import           Czar.Types
import qualified Data.Sequence                       as Seq

data Handler = Handler
    { hdServer   :: Address
    , hdGraphite :: Address
    , hdTags     :: [String]
    , hdDebug    :: Bool
    }

instance CommonOptions Handler where
    debug = hdDebug

program :: ParserInfo Handler
program = info (helper <*> parser) $
       fullDesc
    <> progDesc "Start the Czar Server"
    <> header "czar-server - a test for optparse-applicative"
  where
    parser = Handler
        <$> addressOption "server" defaultHandler
                "Czar Server address to connect to"

        <*> addressOption "graphite" "unix://graphite.sock"
                "Graphite address to write metrics to"

        <*> stringsOption "tags" ["*"]
                "Tags to subscribe to"

        <*> debugSwitch

main :: IO ()
main = runProgram program $ \Handler{..} -> do
    logInfo "starting graphite handler ..."
    connect hdServer $ do
        logPeerTX $ "sending subscription for " ++ show hdTags
        send $ sub hdTags
        continue
  where
    sub = Subscription
        "graphite"
        (Just "Graphite Handler")
        . Seq.fromList
        . map fromString

    continue = receive yield

    yield (E evt) = logPeerRX (show $ E.key evt) >> continue
    yield Syn     = logPeerRX "SYN" >> send Ack >> logPeerTX "ACK" >> continue
    yield _       = logPeerRX "FIN"
