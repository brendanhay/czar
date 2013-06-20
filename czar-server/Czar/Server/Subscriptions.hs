{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Czar.Server.Subscriptions
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Czar.Server.Subscriptions where

import Control.Applicative
import Data.ByteString     (ByteString)
import Data.MultiBiMap     (MultiBiMap)
import Data.Hashable
import Data.IORef

import qualified Data.MultiBiMap as M



type Tag  = ByteString --  deriving (Eq, Hashable)
type Conn = ByteString -- deriving (Eq, Hashable)

newtype Subs = Subs (IORef (MultiBiMap Conn Tag))

showSubs :: Subs -> IO String
showSubs (Subs ref) = show <$> readIORef ref

emptySubs :: IO Subs
emptySubs = Subs <$> newIORef M.empty

subscribe :: Conn -> [Tag] -> Subs -> IO ()
subscribe conn tags (Subs ref) = modifyIORef_ ref $ M.insert conn tags

unsubscribe :: Conn -> Subs -> IO ()
unsubscribe conn (Subs ref) = modifyIORef_ ref (M.delete conn)

subscribers :: [Tag] -> Subs -> IO [Conn]
subscribers tags (Subs ref) =
    (\m -> concatMap (`M.inverse` m) tags) <$> readIORef ref

modifyIORef_ :: IORef a -> (a -> a) -> IO ()
modifyIORef_ ref f = atomicModifyIORef' ref $ \x -> (f x, ())
