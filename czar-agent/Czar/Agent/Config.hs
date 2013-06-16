{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections   #-}

module Czar.Agent.Config where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Error
import Data.Configurator
import Data.Configurator.Types
import Data.List               (isSuffixOf)
import Data.Text               (Text)
import Data.Time.Clock.POSIX
import System.Directory
import System.FilePath

import qualified Data.HashMap.Lazy as H
import qualified Data.Text as T

newtype Tag = Tag Text
newtype Key = Key Text

data Metric
    = Integer Integer
    | Float Double

data Attr = Attr Text Text

data Event = Event
    { evtTime    :: POSIXTime
    , evtKey     :: Key
    , evtDesc    :: Text
    , evtTags    :: [Tag]
    , evtMetrics :: [Metric]
    , evtAttrs   :: [Attr]
    }

data Message = Message
    { msgHost    :: Text
    , msgEvents :: [Event]

extension :: String
extension = ".cfg"

command :: Text
command = ".command"

check :: Name -> Config -> IO (Async ())
check cmd cfg = do
    mval <- lookup cfg key
    
  where
    key = fromMaybe
        (error $ "Invalid check command: " ++ T.unpack cmd)
        (T.stripSuffix command cmd)

groups :: Config -> IO [Name]
groups cfg = filter (command `T.isSuffixOf`) . H.keys <$> getMap cfg

loadConfig :: [FilePath] -> IO (Config, ThreadId)
loadConfig paths = mapM expand paths >>= autoReload autoConfig . concat
  where
    expand p = do
        c <- check p
        case c of
            (True, True, False) -> map Required <$> contents p
            (True, False, True) -> return [Required p]
            _                   -> return [Optional p]

    check x = (,,)
        <$> (readable <$> getPermissions x)
        <*> doesDirectoryExist x
        <*> doesFileExist x

    contents d = map (d </>) . filter config <$> getDirectoryContents d

    config p
        | extension `isSuffixOf` p = True
        | otherwise                = False
