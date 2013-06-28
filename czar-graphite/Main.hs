{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main (main) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Options

import Czar.Internal.Protocol.Subscription
import Czar.Internal.Protocol.Tag
import Czar.Log
import Czar.Protocol
import Czar.Socket

import qualified Data.ByteString.Char8 as BS
import qualified Data.Sequence         as Seq

defineOptions "HandlerOpts" $ do
    stringOption "hdServer" "server" defaultHandler "Server for the handler to connect to."

main :: IO ()
main = runCommand $ \HandlerOpts{..} _ -> runScript $ do
    setLogging

    addr <- parseAddr hdServer

    logInfo "Starting handler ..."

    scriptIO . connect addr $ do

        let sub = Subscription
                      "graphite"
                      (Just "Graphite Handler")
                      (Seq.fromList [Tag "*"])
        send sub

        forever $ eitherReceive logError $ \evt -> do
            liftIO $ print (evt :: Event)
