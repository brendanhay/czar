{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternGuards   #-}

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
      SocketContext

    -- * Default Addresses
    , defaultAgent
    , defaultServer
    , defaultHandler

    -- * Constructors
    , listen
    , connect
    , accept

    -- * Context Operations
    , send
    , receive
    , eitherReceive
    , close

    -- * Parser
    , parseAddr
    ) where

import Control.Applicative           hiding ((<|>))
import Control.Concurrent
import Control.Error
import Control.Monad.CatchIO
import Control.Monad.IO.Class
import Control.Monad.Reader
import Network.Socket                hiding (listen, connect, accept, send, close)
import System.Directory
import System.FilePath
import System.IO.Unsafe                     (unsafePerformIO)
import Text.ParserCombinators.Parsec hiding (try)

import Czar.Log
import Czar.Protocol

import qualified Network.Socket                 as Sock hiding (recv)
import qualified Network.Socket.ByteString.Lazy as Sock

type SocketContext = ReaderT Socket

-- FIXME: handle recv "" empty string, disconnections, heartbeats

defaultAgent, defaultServer, defaultHandler :: String
defaultAgent   = "unix://czar-agent.sock"
defaultHandler = "unix://czar-handler.sock"
defaultServer  = "tcp://127.0.0.1:5555"

listen :: MonadCatchIO m => SockAddr -> SocketContext m a -> m a
listen addr ctx = do
    sock <- liftIO $ Sock.socket (family addr) Stream Sock.defaultProtocol
    liftIO $ do
        Sock.setSocketOption sock ReuseAddr 1
        Sock.bind sock addr
        Sock.listen sock 5
    logInfo $ "Listening on " ++ show addr
    withSocket sock ctx `finally` releaseAddr addr

connect :: MonadCatchIO m => SockAddr -> SocketContext m a -> m a
connect addr ctx = do
    sock <- liftIO $ Sock.socket (family addr) Stream Sock.defaultProtocol
    liftIO $ Sock.connect sock addr
    logInfo $ "Connected to " ++ show addr
    withSocket sock ctx

accept :: MonadCatchIO m => SocketContext IO () -> SocketContext m ThreadId
accept ctx = forever $ do
    parent     <- ask
    (child, _) <- liftIO $ Sock.accept parent
    liftIO . forkIO $ withSocket child ctx

withSocket :: MonadCatchIO m => Socket -> SocketContext m a -> m a
withSocket sock action = runReaderT (action `finally` close' sock) sock

send :: (Protocol a, MonadCatchIO m) => a -> SocketContext m ()
send pb = ask >>= liftIO . (`Sock.sendAll` messagePut pb)

receive :: (Protocol a, MonadCatchIO m) => SocketContext m (Either String a)
receive = do
    sock <- ask
    bs   <- liftIO $ Sock.recv sock 2048
    logInfo $ "Received: " ++ show bs
    case messageGet bs of
        Right (x, _) -> return $! Right x
        Left msg     -> close >> return (Left msg)

eitherReceive :: (Protocol a, MonadCatchIO m)
              => (String -> SocketContext m b)
              -> (a -> SocketContext m b)
              -> SocketContext m b
eitherReceive f g = receive >>= either f g

close :: MonadCatchIO m => SocketContext m ()
close = ask >>= close'

parseAddr :: Monad m => String -> EitherT String m SockAddr
parseAddr str = fmapLT f . hoistEither $ parse addrGeneric "" str
  where
    f = (str ++) . (" " ++) . show

--
-- Internal
--

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
    s <- (string "tcp" <|> string "unix") <* string "://"
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

close' :: MonadCatchIO m => Socket -> m ()
close' sock = liftIO $ do
    p <- Sock.isConnected sock
    when p $ Sock.close sock
