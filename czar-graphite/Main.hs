{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

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

import           Control.Error
import qualified Czar.Internal.Protocol.Event        as E
import           Czar.Internal.Protocol.Subscription
import           Czar.Log
import           Czar.Protocol
import           Czar.Socket
import qualified Data.Sequence                       as Seq
import           Options

defineOptions "HandlerOpts" $ do
    stringOption "hdServer" "server" defaultHandler
        "Server for the handler to connect to."

    boolOption "hdVerbose" "verbose" False
        "Be really loud."

main :: IO ()
main = runCommand $ \HandlerOpts{..} _ -> scriptLogging hdVerbose $ do
    addr <- parseAddr hdServer

    logInfo "starting graphite handler ..."

    scriptIO . connect addr $ do
        logPeerTX $ "sending subscription for " ++ show tags

        send sub

        continue
  where
    sub = Subscription
        "graphite"
        (Just "Graphite Handler")
        (Seq.fromList tags)

    tags = ["*"]

    continue = receive yield

    yield (E evt) = logPeerRX (show $ E.key evt) >> continue
    yield Syn     = logPeerRX "SYN" >> send Ack >> logPeerTX "ACK" >> continue
    yield _       = logPeerRX "FIN"
