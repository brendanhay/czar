{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Czar.Agent.Socket (
      pairSockets
    , writeSocket
    , validateSocket
    , whenSocket
    , unlessSocket
    ) where

import Control.Concurrent.STM
import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Czar.Agent.Log
import Data.List              (isPrefixOf)
import Data.Monoid
import System.Directory
import System.ZMQ3.Monadic

import qualified Control.Exception     as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T

pairSockets :: String -> String -> (BS.ByteString -> BS.ByteString) -> IO ()
pairSockets laddr raddr handle =
    run `E.finally` (whenSocket laddr . removeFile $ stripScheme laddr)
  where
    run = runZMQ $ do
        chan   <- atomicallyM newTChan
        remote <- pushSocket raddr
        local  <- pullSocket laddr

        async . forever $ do
            bs <- receive local
            logInfo $ "Received " <> BS.unpack bs <> " from " <> laddr
            atomicallyM $ writeTChan chan bs

        forever $ do
            bs <- atomicallyM $ readTChan chan
--logInfo $ "Sent " <> BS.unpack bs <> " to " <> raddr
            send remote [] $ handle bs

writeSocket :: String -> BS.ByteString -> ZMQ z ()
writeSocket addr bs = do
    sock <- pushSocket addr
    send sock [] bs

pushSocket :: String -> ZMQ z (Socket z Push)
pushSocket addr = do
    sock <- socket Push
    connect sock addr
    logInfo $ "Connected to " ++ addr
    return sock

pullSocket :: String -> ZMQ z (Socket z Pull)
pullSocket addr = do
   sock <- socket Pull
   bind sock addr
   logInfo $ "Listening on " ++ addr
   return sock

whenSocket, unlessSocket :: MonadIO m => String -> m () -> m ()
whenSocket   = (when `pathExistsM`)
unlessSocket = (unless `pathExistsM`)

validateSocket :: String -> Script ()
validateSocket addr = unless ("ipc://" `isPrefixOf` addr) $ do
    throwT $ addr ++ " does not start with the required ipc:// scheme"

pathExistsM :: MonadIO m => (Bool -> a -> m ()) -> String -> a -> m ()
pathExistsM cond str f =
    liftIO (doesFileExist $ stripScheme str) >>= flip cond f

stripScheme :: String -> String
stripScheme = T.unpack . last . T.splitOn "://" . T.pack

atomicallyM :: MonadIO m => STM a -> m a
atomicallyM = liftIO . atomically
