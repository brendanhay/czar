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

-- module Czar.Socket (
--     -- * ReaderT Context
--       SocketContext

--     -- * Default Addresses
--     , defaultAgent
--     , defaultServer
--     , defaultHandler

--     -- * Constructors
--     , listen
--     , connect
--     , accept

--     -- * Context Operations
--     , send
--     , receive
--     , eitherReceive
--     , close

--     -- * Parser
--     , parseAddr
--     ) where

module Czar.Socket where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.Timer
import Control.Error
import Control.Monad.CatchIO
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Network.Socket                hiding (listen, connect, accept, send, close)
import System.Directory
import System.FilePath
import System.IO.Unsafe                     (unsafePerformIO)
import Text.ParserCombinators.Parsec hiding ((<|>), try)

import Czar.Log
import Czar.Protocol
import Czar.Internal.Protocol.Ping
import Czar.Internal.Protocol.Pong

import qualified Network.Socket                 as Sock hiding (recv)
import qualified Network.Socket.ByteString.Lazy as Sock
import qualified Text.ParserCombinators.Parsec  as P

import qualified Data.ByteString.Lazy as LBS

defaultAgent, defaultServer, defaultHandler :: String
defaultAgent   = "unix://czar-agent.sock"
defaultHandler = "unix://czar-handler.sock"
defaultServer  = "tcp://127.0.0.1:5555"

-- FIXME: handle recv "" empty string, disconnections, heartbeats

class Sender m a where
    send :: (Protocol msg, MonadCatchIO m) => msg -> m ()

class Receiver m where
    receive :: (Protocol msg, MonadCatchIO m, MonadPlus m) => m (Either String msg)

newtype Server m a = Server { runServer :: StateT (Socket, [Async ()]) m a }
    deriving (Monad, MonadCatchIO, MonadIO)

instance MonadCatchIO m => MonadState (Socket, [Async ()]) (Server m) where
    get = Server get
    put = Server . put

newtype Node m a = Node { runNode :: ReaderT Socket m a }
    deriving (Monad, MonadCatchIO, MonadIO, MonadPlus, MonadReader Socket)

instance MonadCatchIO m => Sender (Node m) a where
    send pb = ask >>= liftIO . (`Sock.sendAll` messagePut pb)

instance MonadCatchIO m => Receiver (Node m) where
    receive = do
        sock <- ask
        bs  <- liftIO $ Sock.recv sock 2048

        logInfo $ "Node received: " ++ show bs

        return $ Left "error"

        attempt (\(Ping _) -> send (Pong $ Just "For you.") >> receive) bs

newtype Root m a = Root { runRoot :: ReaderT (Socket, Timer) m a }
    deriving (Monad, MonadCatchIO, MonadIO, MonadPlus, MonadReader (Socket, Timer))

instance MonadCatchIO m => Sender (Root m) a where
    send pb = ask >>= liftIO . (`Sock.sendAll` messagePut pb) . fst

instance MonadCatchIO m => Receiver (Root m) where
    receive = do
        (sock, timer) <- ask
        bs            <- liftIO $ Sock.recv sock 2048

        logInfo $ "Root received: " ++ show bs

        attempt (\(Pong _) -> liftIO (resetTimer timer) >> receive) bs

attempt :: (MonadPlus m, Protocol a, Protocol b)
        => (a -> m (Either String b))
        -> LBS.ByteString
        -> m (Either String b)
attempt action bs = mplus
    (case messageGet bs of
          Left e       -> return $ Left e
          Right (x, _) -> action x)
    (return $ fst `fmap` messageGet bs)

listen :: MonadCatchIO m => SockAddr -> Server m a -> m a
listen addr server = do
    sock     <- open
    (res, _) <- runStateT (runServer server `finally` cleanup sock) (sock, [])

    -- do something with the async timers here
    return res
  where
    open = liftIO $ do
        sock <- Sock.socket (family addr) Stream Sock.defaultProtocol

        Sock.setSocketOption sock ReuseAddr 1
        Sock.bind sock addr
        Sock.listen sock 5

        logInfo $ "Listening on " ++ show addr

        return sock

    cleanup sock = close sock >> releaseAddr addr

accept :: MonadCatchIO m => Root IO a -> Server m ()
accept root = forever $ do
    (sock, as) <- get
    async'     <- fork sock
    put $! (sock, async' : as)
  where
    fork parent = liftIO $ do
        (child, _) <- Sock.accept parent

        timer <- startTimer 5 $ do
            logInfo "Sending ping ..."
            let proto = messagePut $ Ping (Just "Hello, friend.")
            liftIO $ print proto
            Sock.sendAll child proto
            threadSleep 5
            logError "Heartbeat reponse time exceeded ..."
            close child

        async . void $
            runReaderT (runRoot root `finally` close child) (child, timer)

connect :: MonadCatchIO m => SockAddr -> Node m a -> m a
connect addr node = open >>= run
  where
    open = liftIO $ do
        sock <- Sock.socket (family addr) Stream Sock.defaultProtocol
        Sock.connect sock addr

        logInfo $ "Connected to " ++ show addr

        return sock

    run sock = runReaderT (runNode node `finally` close sock) sock

eitherReceive :: (Protocol msg, MonadCatchIO m, MonadPlus m, Receiver m)
              => (String -> m b)
              -> (msg -> m b)
              -> m b
eitherReceive f g = receive >>= either f g

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

close :: MonadCatchIO m => Socket -> m ()
close sock = liftIO $ do
    p <- Sock.isConnected sock
    when p $ Sock.close sock
