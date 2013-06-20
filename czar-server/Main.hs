{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

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

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Czar.Log
import Czar.Server.Subscriptions
import Czar.Socket
import Data.Foldable                   (toList)
import Network.BSD              hiding (hostName)
import Options

import qualified System.ZMQ3.Monadic as Z

import qualified Control.Exception     as E
import qualified Czar.Protocol         as P
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T

import qualified Czar.Internal.Protocol.Subscription as PSub

import qualified Data.Sequence as Seq

defineOptions "ServerOpts" $ do
    stringOption "srvListen"  "listen"  defaultServer ""
    stringOption "srvPublish" "publish" defaultHandler ""

main :: IO ()
main = runCommand $ \ServerOpts{..} _ -> runScript $ do
    setLogging

    luri <- parseUri srvListen
    puri <- parseUri srvPublish
    chan <- liftIO $ atomically newTChan

    logInfo "Starting server ..."

    tryCatchIO (mapM cleanup [luri, puri]) $ do
        h <- handlers puri chan
        wait h

  --      waitEither_ l p

1) Use TCP
2) Async and start a state machine representing the handler from servers' pov


  where
    handlers uri chan = async $ runZMQ $ do
        subs <- liftIO emptySubs
        sock <- bindServerForHandlers uri

        forever $ do
            bs <- receiveMulti sock

            case bs of
                [ident, payload] ->
                    case P.messageGet payload of
                        Right (s :: P.Subscription) -> do
                            liftIO $ print s

                            liftIO $ subscribe ident
                                (map (P.utf8BS . P.name) $ toList $ P.tags s) subs
    
                            sendMulti sock [ident, "OK"]
                            -- Z.async $ forever $ do
                            --     liftIO $ threadDelay 1000000
                            --     sendMulti sock [ident, "HEART"]
                        Left msg -> error msg
                other ->
                    logError $ "Received unknown subscription request: " ++ show other

            liftIO (showSubs subs) >>= logInfo

            -- send heartbeat queries
--            liftIO . atomically $ writeTChan chan bs

    -- publish uri chan = async $ runZMQ $ do
    --     sock <- serverPublish uri
    --     forever $ do
    --         bs <- liftIO . atomically $ readTChan chan
    --         sendMulti sock ["", bs]
    --         logInfo $ "Sent " ++ BS.unpack bs ++ " to " ++ show uri


-- - Should switch to local tcp instead of ipc:
--   ensures no simultaneous agents etc due to port binding

-- Agent:
--   knows server address

-- Server:
--   ability to run N servers inproc, with one recv socket+channel
--   which distributes internally

--   connected to by multiple agents
--   - Push -> Pull

--   connected to by multiple handlers
--   - Server/Router <-> Handler/Dealer

--   receive subscription tags from handler and generate a key,
--   storing the subscription/key/socket mapping

--   server sends heartbeats per subscription key
--   and expects a reply confirming the key

-- Handler:
--   - connect and send subscription tags
--   - server stores subscription tags with identity
--   -- - heartbeats are sent over this connection
--   -- - heartbeat failure causes both connections to be terminated by the server
