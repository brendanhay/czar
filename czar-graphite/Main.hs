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

import Control.Error
import Control.Monad.IO.Class
import Options

import Czar.Internal.Protocol.Subscription
import Czar.Internal.Protocol.Tag
import Czar.Log
import Czar.Protocol
import Czar.Socket

import qualified Data.Sequence as Seq

defineOptions "HandlerOpts" $ do
    stringOption "hdServer" "server" defaultHandler
        "Server for the handler to connect to."

main :: IO ()
main = runCommand $ \HandlerOpts{..} _ -> scriptLogging $ do
    addr <- parseAddr hdServer

    logInfo "Starting graphite handler ..."

    scriptIO . connect addr $ do
        send sub
        loop
  where
    sub = Subscription
        "graphite"
        (Just "Graphite Handler")
        (Seq.fromList [Tag "*"])

    loop = receive handle

    handle (E evt) = liftIO (print evt) >> loop
    handle _       = return ()
