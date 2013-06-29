{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
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

module Czar.Socket (
    -- * ReaderT Context
      Context

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
    , sendHeartbeats
    , close
    , fork

    -- * Logging
    , logPeer
    , logPeerTX
    , logPeerRX

    -- * Parser
    , parseAddr
    ) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.Timer
import Control.Error
import Control.Monad.CatchIO
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List                            (isInfixOf)
import Network.Socket                hiding (listen, connect, accept, send, close, socket)
import System.Directory
import System.FilePath
import System.IO.Unsafe                     (unsafePerformIO)
import Text.ParserCombinators.Parsec hiding ((<|>), try)
import Text.Printf

import Czar.Log
import Czar.Protocol

import qualified Network.Socket                 as Sock hiding (recv)
import qualified Network.Socket.ByteString.Lazy as Sock
import qualified Text.ParserCombinators.Parsec  as P

defaultAgent, defaultServer, defaultHandler :: String
defaultAgent   = "unix://czar-agent.sock"
defaultHandler = "tcp://127.0.0.1:5556"
defaultServer  = "tcp://127.0.0.1:5555"

newtype Context m a = Context { runCtx :: ReaderT Socket m a }
    deriving (Monad, MonadCatchIO, MonadIO, MonadReader Socket)

listen :: MonadCatchIO m => SockAddr -> Context m a -> m a
listen addr ctx = bracket open cleanup accept
  where
    open = liftIO $ do
        sock <- Sock.socket (family addr) Stream Sock.defaultProtocol

        Sock.setSocketOption sock ReuseAddr 1
        Sock.bind sock addr
        Sock.listen sock 5

        logAddr INFO addr "Listening"

        return sock

    cleanup sock = close sock >> releaseAddr addr

    accept sock = forever $ do
        (child, _) <- liftIO $ Sock.accept sock
        runReaderT (runCtx ctx) child

connect :: MonadCatchIO m => SockAddr -> Context m a -> m a
connect addr ctx = bracket open close $ runReaderT action
  where
    open = liftIO $ do
        sock <- Sock.socket (family addr) Stream Sock.defaultProtocol

        Sock.connect sock addr

        return sock

    action =  runCtx (logPeerTX "Connected") >> runCtx ctx

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

sendHeartbeats :: MonadIO m => Seconds -> Context m Timer
sendHeartbeats n = do
    sock <- socket
    peer <- peerName
    name <- sockName

    liftIO . startTimer n $ do
        logTX name peer "SYN"
        Sock.sendAll sock $ messagePut Syn

        threadSleep n

        logRX peer name "TIMEOUT"
        Sock.sendAll sock $ messagePut Fin

        close sock

fork :: MonadCatchIO m => Context IO a -> Context m (Async a)
fork ctx = do
    sock <- socket
    liftIO . async $ runReaderT (runCtx ctx) sock

socket :: Monad m => Context m Socket
socket = ask

close :: MonadCatchIO m => Socket -> m ()
close sock = liftIO $ do
    p <- Sock.isConnected sock
    when p $ Sock.close sock

parseAddr :: Monad m => String -> EitherT String m SockAddr
parseAddr str = fmapLT f . hoistEither $ parse addrGeneric "" str
  where
    f = (str ++) . (" " ++) . show

-- FIXME: Tidy this shit up

logPeer prio msg = peerName >>= \peer -> logAddr prio peer msg

logPeerTX msg = do
    peer <- peerName
    name <- sockName
    logTX peer name msg

logPeerRX msg = do
    peer <- peerName
    name <- sockName
    logRX name peer msg

logTX :: MonadIO m => SockAddr -> SockAddr -> String -> m ()
logTX from to msg =
    logM DEBUG $ printf "[%s -> %s] %s" (show from) (show to) msg

logRX :: MonadIO m => SockAddr -> SockAddr -> String -> m ()
logRX to from msg =
    logM DEBUG $ printf "[%s <- %s] %s" (show to) (show from) msg

logAddr :: MonadIO m => Priority -> SockAddr -> String -> m ()
logAddr prio addr = logM prio . (peer ++)
  where
    peer = case show addr of
        "" -> ""
        s  -> printf "[%s] " s

--
-- Internal
--

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
family (SockAddrInet _ _)      = AF_INET
family (SockAddrInet6 _ _ _ _) = AF_INET6
family (SockAddrUnix _)        = AF_UNIX

addrGeneric :: GenParser Char s SockAddr
addrGeneric = do
    s <- (string "tcp" P.<|> string "unix") <* string "://"
    case s of
        "tcp"  -> addrTcp
        "unix" -> addrUnix
        _      -> fail "Unable to parse addr"

addrTcp :: GenParser Char s SockAddr
addrTcp = do
    h <- unsafePerformIO . inet_addr <$> addrPath
    p <- addrPort
    return $! SockAddrInet p h

addrUnix :: GenParser Char s SockAddr
addrUnix = do
    p <- addrPath
    if isValid p
     then return $! SockAddrUnix p
     else fail $ "Invalid unix:// socket path: " ++ p

addrPort :: GenParser Char s PortNumber
addrPort = f <$> (char ':' *> (read <$> many1 digit) <* eof)
  where
    f x = fromIntegral (x :: Integer)

addrPath :: GenParser Char s String
addrPath = many1 $ noneOf "!@#$%^&*()=\\/|\"',?:"
