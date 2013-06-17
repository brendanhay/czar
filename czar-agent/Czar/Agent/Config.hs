{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Czar.Agent.Config where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Configurator
import Data.Configurator.Types
import Data.List               (isSuffixOf)
import Data.Maybe
import Data.Monoid
import Data.Text               (Text)
import Data.Time.Clock.POSIX
import System.Directory
import System.FilePath

import qualified Data.HashMap.Lazy as H
import qualified Data.Text         as T

data Check = Check
    { chkName     :: Text
    , chkDesc     :: Maybe Text
    , chkCommand  :: Text
    , chkInterval :: Int
    , chkTags     :: [Text]
    } deriving (Eq, Ord, Show)

extension :: String
extension = ".cfg"

command :: Text
command = ".command"

loadConfig :: [FilePath] -> IO Config
loadConfig paths = mapM expand paths >>= load . concat
  where
    expand p = do
        a <- attrs p
        case a of
            (True, True, False) -> map Required <$> contents p
            (True, False, True) -> return [Required p]
            _                   -> return [Optional p]

    attrs x = (,,)
        <$> (readable <$> getPermissions x)
        <*> doesDirectoryExist x
        <*> doesFileExist x

    contents d = map (d </>) . filter config <$> getDirectoryContents d

    config p
        | extension `isSuffixOf` p = True
        | otherwise                = False

parseChecks :: Config -> IO [Check]
parseChecks cfg = names >>= mapM (parseCheck cfg)
  where
    names = filter (command `T.isSuffixOf`) . H.keys <$> getMap cfg

parseCheck :: Config -> Name -> IO Check
parseCheck cfg name = Check key
    <$> optional ".description"
    <*> required command
    <*> required ".interval"
    <*> (do List vs <- required ".tags"; return $ mapMaybe convert vs)
  where
    required :: Configured a => Name -> IO a
    required n = do
      m <- optional n
      return $ fromMaybe
          (error . T.unpack $ "Missing check field " <> key <> n)
          m

    optional :: Configured a => Name -> IO (Maybe a)
    optional = lookup cfg . (key <>)

    key = fromMaybe
        (error $ "Invalid check command: " ++ T.unpack name)
        (T.stripSuffix command name)

forkChecks :: Integer -> [Check] -> IO ()
forkChecks splay cs = zipWithM fork cs steps >>= mapM_ link
  where
    fork Check{..} n = async $ do
        threadDelay n
        forever $ do
            threadDelay $ chkInterval * 1000000
            putStrLn . T.unpack $ "Running " <> chkName <> ":" <> chkCommand

    steps = scanl1 (+) . repeat $ fromIntegral splay * 10000
