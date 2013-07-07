{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- |
-- Module      : Czar.Socket
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Czar.Socket
    -- * ReaderT Context
    ( Context

    -- * Default Addresses
    , defaultAgent
    , defaultServer
    , defaultHandler

    -- * Constructors
    , listen
    , connect

    -- * Context Operations
    , receive
    , send
    , heartbeat
    , finish

    -- * Fork Contexts
    , forkContext
    , forkContextFinally

    -- * Logging
    , logPeerInfo
    , logPeerTX
    , logPeerRX
    ) where

import           Control.Concurrent
import           Control.Concurrent.Timeout       (Timeout)
import qualified Control.Concurrent.Timeout       as Timeout
import           Control.Monad.CatchIO
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Czar.Log
import           Czar.Protocol
import           Czar.Types
import qualified Network.Socket                 as Sock hiding (recv)
import           Network.Socket                 hiding (listen, connect, accept, send, close, socket)
import qualified Network.Socket.ByteString.Lazy as Sock
import           System.Directory
import           Text.Printf

defaultAgent, defaultServer, defaultHandler :: Address
defaultAgent   = "unix://czar-agent.sock"
defaultHandler = "tcp://127.0.0.1:5556"
defaultServer  = "tcp://127.0.0.1:5555"

newtype Context m a = Context { runCtx :: ReaderT Socket m a }
    deriving (Functor, Monad, MonadCatchIO, MonadIO, MonadReader Socket)

listen :: MonadCatchIO m => Address -> Context IO () -> m a
listen (Address addr) ctx = bracket open cleanup accept
  where
    open = liftIO $ do
        sock <- Sock.socket (family addr) Stream Sock.defaultProtocol

        Sock.setSocketOption sock ReuseAddr 1
        Sock.bind sock addr
        Sock.listen sock 5

        logAddr INFO addr "listening"

        return sock

    cleanup sock = close sock >> releaseAddr addr

    accept sock = forever . liftIO $ do
        (child, _) <- Sock.accept sock
        void $ forkFinally
            (runReaderT (runCtx ctx) child)
            (const $ close child)

connect :: MonadCatchIO m => Address -> Context m a -> m a
connect (Address addr) ctx = bracket open close $ runReaderT action
  where
    open = liftIO $ do
        sock <- Sock.socket (family addr) Stream Sock.defaultProtocol

        Sock.connect sock addr

        return sock

    action = runCtx (logPeerTX "connected") >> runCtx ctx

receive :: MonadCatchIO m => (Payload -> Context m a) -> Context m a
receive action = do
    sock <- socket
    bs   <- liftIO $ Sock.recv sock 2048
    pay  <- messageGet bs
    action pay

send :: (MonadCatchIO m, Protocol a) => a -> Context m ()
send msg = do
    sock <- socket
    liftIO . Sock.sendAll sock $ messagePut msg

heartbeat :: MonadIO m => Seconds -> Context m Timeout
heartbeat n = do
    sock <- socket
    peer <- peerName
    name <- sockName

    Timeout.start n
        (logTX name peer "SYN" >> send' sock Syn)
        (logRX peer name "TIMEOUT" >> send' sock Fin `finally` close sock)
  where
    send' sock = Sock.sendAll sock . messagePut

forkContext :: (Functor m, MonadCatchIO m)
            => Context IO ()
            -> Context m ThreadId
forkContext = (`forkContextFinally` return ())

forkContextFinally :: (Functor m, MonadCatchIO m)
                   => Context IO ()
                   -> Context IO ()
                   -> Context m ThreadId
forkContextFinally ctx cleanup = do
    sock <- socket
    liftIO $ forkFinally
        (runReaderT (runCtx ctx) sock)
        (const $ runReaderT (runCtx cleanup) sock)

socket :: Monad m => Context m Socket
socket = ask

finish :: MonadCatchIO m => Context m ()
finish = socket >>= close

logPeerInfo :: MonadIO m => String -> Context m ()
logPeerInfo msg = peerName >>= \peer -> logAddr INFO peer msg

logPeerTX :: MonadIO m => String -> Context m ()
logPeerTX msg = do
    peer <- peerName
    name <- sockName
    logTX name peer msg

logPeerRX :: MonadIO m => String -> Context m ()
logPeerRX msg = do
    peer <- peerName
    name <- sockName
    logRX name peer msg

logTX :: MonadIO m => SockAddr -> SockAddr -> String -> m ()
logTX from to msg =
    logM DEBUG $ printf "%s -> %s %s" (show from) (show to) msg

logRX :: MonadIO m => SockAddr -> SockAddr -> String -> m ()
logRX to from msg =
    logM DEBUG $ printf "%s <- %s %s" (show to) (show from) msg

logAddr :: MonadIO m => Priority -> SockAddr -> String -> m ()
logAddr prio addr = logM prio . (peer ++)
  where
    peer = case show addr of
        "" -> ""
        s  -> printf "%s " s

--
-- Internal
--

close :: MonadCatchIO m => Socket -> m ()
close sock = liftIO $ do
    p <- Sock.isConnected sock
    when p $ Sock.close sock

peerName :: MonadIO m => Context m SockAddr
peerName = socket >>= liftIO . getPeerName

sockName :: MonadIO m => Context m SockAddr
sockName = socket >>= liftIO . getSocketName

releaseAddr :: MonadIO m => SockAddr -> m ()
releaseAddr (SockAddrUnix path) = liftIO $ do
    p <- doesFileExist path
    when p $ removeFile path
releaseAddr _ = return ()

family :: SockAddr -> Family
family SockAddrInet{..}  = AF_INET
family SockAddrInet6{..} = AF_INET6
family SockAddrUnix{..}  = AF_UNIX
