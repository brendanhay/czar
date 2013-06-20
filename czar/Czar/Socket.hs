{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Czar.Socket where

import Control.Applicative           hiding ((<|>))
import Control.Arrow
import Control.Error                 hiding (left)
import Control.Exception                    (SomeException)
import Control.Monad.CatchIO
import Control.Monad.IO.Class
import Czar.Log
import Data.Word
import System.FilePath
import System.Directory
import System.ZMQ3.Monadic           hiding (async)
import Text.ParserCombinators.Parsec hiding (try)

data Uri
    = Tcp String Word16
    | Ipc FilePath

instance Show Uri where
    show (Tcp host port) = "tcp://" ++ host ++ ":" ++ show port
    show (Ipc path)      = "ipc://" ++ path

defaultAgent   = "ipc://czar-agent.sock"
defaultServer  = "tcp://127.0.0.1:5555"
defaultHandler = "tcp://127.0.0.1:5556"

connectAgentToAgent = connectSocket Push

bindAgentForAgents = bindSocket Pull

connectAgentToServer = connectSocket Push

bindServerForAgents = bindSocket Pull

bindServerForHandlers = bindSocket Router

connectHandlerToServer = connectSocket Dealer

bindSocket :: (SocketType t, Receiver t) => t -> Uri -> ZMQ z (Socket z t)
bindSocket typ uri =
    let addr = show uri
    in do sock <- socket typ
          bind sock addr
          logInfo $ "Listening on " ++ addr
          return sock

connectSocket :: (SocketType t, Sender t) => t -> Uri -> ZMQ z (Socket z t)
connectSocket typ uri =
    let addr = show uri
    in do sock <- socket typ
          connect sock addr
          logInfo $ "Connected to " ++ addr
          return sock

ensure :: MonadIO m => Uri -> Bool -> EitherT String m ()
ensure uri exists =
    let addr = show uri
    in do p <- connected uri
          case (exists, isJust p) of
              (True, False) -> throwT $ addr ++ " doesn't exist"
              (False, True) -> throwT $ addr ++ " already exists"
              _             -> return ()

tryCatchIO :: (Functor m, MonadCatchIO m) => m a -> m b -> EitherT String m b
tryCatchIO after = fmapLT show . try_ after
  where
    try_ :: (Functor m, MonadCatchIO m) => m a -> m b -> EitherT SomeException m b
    try_ f g = EitherT $ try (g `finally` f)

cleanup :: MonadIO m => Uri -> m ()
cleanup uri = connected uri >>= maybe (return ()) (liftIO . removeFile)

connected :: MonadIO m => Uri -> m (Maybe FilePath)
connected (Tcp _ _)  = return Nothing
connected (Ipc path) = do
    p <- liftIO $ doesFileExist path
    return $ if p then Just path else Nothing

parseUri :: Monad m => String -> EitherT String m Uri
parseUri str =
    hoistEither . left ((str ++) . (" " ++) . show) $ parse uriGeneric "" str

uriGeneric :: GenParser Char s Uri
uriGeneric = do
    s <- (string "tcp" <|> string "ipc") <* string "://"
    case s of
        "tcp" -> Tcp <$> uriPath <*> uriPort
        "ipc" -> uriIpc
        _     -> fail "Unable to parse uri"

uriIpc :: GenParser Char s Uri
uriIpc = do
    p <- uriPath
    if isValid p
      then return $! Ipc p
      else fail $ "Invalid ipc:// socket path: " ++ p

uriPath :: GenParser Char s String
uriPath = many1 $ noneOf "!@#$%^&*()=\\/|\"',?:"

uriPort :: GenParser Char s Word16
uriPort = char ':' *> (read <$> many1 digit) <* eof
