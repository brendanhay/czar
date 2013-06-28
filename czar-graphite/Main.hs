{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import Control.Error
import Control.Monad
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

        forever $ eitherReceive logError $ \evt -> do
            liftIO $ print (evt :: Event)
  where
    sub = Subscription
        "graphite"
        (Just "Graphite Handler")
        (Seq.fromList [Tag "*"])
