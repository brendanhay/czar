{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- |
-- Module      : Czar.Agent.Check
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Czar.Agent.Check
    ( Check(..)
    , loadChecks
    , forkChecks
    ) where

import           Prelude                      hiding (lookup)

import           Control.Applicative          ((<$>), (<*>))
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Configurator
import           Data.Configurator.Types
import           Data.List                    (isSuffixOf)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                    (Text)
import           System.Directory
import           System.FilePath

import qualified Data.HashMap.Lazy            as H
import qualified Data.Sequence                as Seq
import qualified Data.Text                    as T
import qualified Data.Text.Lazy.Encoding      as LE
import qualified Data.Text.Lazy               as LT

import           Czar.Protocol

import qualified Czar.Internal.Protocol.Event as E

data Check = Check
    { chkName     :: !Text
    , chkDesc     :: !(Maybe Text)
    , chkCommand  :: !Text
    , chkInterval :: !Int
    , chkTags     :: ![Text]
    } deriving (Eq, Ord, Show)

loadChecks :: MonadIO m => [FilePath] -> m [Check]
loadChecks paths = liftIO $ loadConfig paths >>= parseChecks

forkChecks :: MonadIO m => Integer -> TQueue Event -> [Check] -> m ()
forkChecks splay queue cs = liftIO $ zipWithM fork cs steps >>= mapM_ link
  where
    fork Check{..} n = async $ do
        threadDelay n
        forever $ do
            threadDelay $ chkInterval * 1000000

            let evt = E.Event 0 (enc chkName) "key" (enc <$> chkDesc) (Seq.fromList []) (Seq.fromList []) (Seq.fromList [])

            atomically $ writeTQueue queue evt

    steps = scanl1 (+) . repeat $ fromIntegral splay * 10000
    enc   = Utf8 . LE.encodeUtf8 . LT.fromStrict

--
-- Internal
--

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
