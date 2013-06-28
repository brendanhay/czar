{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Control.Concurrent
import Control.Concurrent.Async.Extensions
import Control.Concurrent.STM
import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Options

import Czar.Log
import Czar.Server.Routing
import Czar.Socket

defineOptions "ServerOpts" $ do
    stringOption "srvListen" "listen" defaultServer
        ""

    stringOption "srvHandlers" "publish" defaultHandler
        ""

main :: IO ()
main = runCommand $ \ServerOpts{..} _ -> scriptLogging $ do
    hds <- parseAddr srvHandlers
    lst <- parseAddr srvListen

    logInfo "Starting server ..."

    scriptIO $ do
        routes <- liftIO emptyRoutes
        linkedRace_
            (handlers hds routes)
            (agents lst routes)
  where
    handlers addr routes = listen addr . accept $ do
        logInfo "Accepted handler connection"
        eitherReceive logError $ \s -> do
            tid   <- liftIO myThreadId
            queue <- liftIO $ subscribe s tid routes
            forever $ do
                evt <- liftIO . atomically $ readTQueue queue
                send evt
            close

    agents addr routes = listen addr . accept $ do
        logInfo "Accepted agent connection"
        forever $ eitherReceive logError $ \evt ->
            liftIO $ notify evt routes
        close
