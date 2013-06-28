{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Czar.Server.Routing
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Czar.Server.Routing where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.MultiBiMap        (MultiBiMap)
import Data.Hashable
import Data.HashMap.Strict    (HashMap)
import Data.IORef
import Data.Foldable          (toList)
import Czar.Log
import Czar.Protocol          (Event, Subscription, Tag)

import qualified Data.HashMap.Strict                 as Queues
import qualified Data.MultiBiMap                     as Index

import qualified Czar.Internal.Protocol.Event        as E
import qualified Czar.Internal.Protocol.Subscription as S
import qualified Czar.Internal.Protocol.Tag          as T
import qualified Czar.Protocol                       as P

type Key a = (Eq a, Hashable a, Show a)

data Table a = Table (MultiBiMap a Tag) (HashMap a (TQueue Event))

newtype Routes a = Routes (IORef (Table a))

-- FIXME: Should be STM or MVar to ensure notify doesn't have something added
--        while pushing to existing?

instance Hashable Tag where
    hashWithSalt salt (T.Tag n) = hashWithSalt salt $ P.utf8ToBS n

emptyRoutes :: Key a => IO (Routes a)
emptyRoutes = Routes <$> newIORef (Table Index.empty Queues.empty)

subscribe :: Key a => Subscription -> a -> Routes a -> IO (TQueue Event)
subscribe sub key (Routes ref) = do
    q <- newTQueueIO
    modifyIORef_ ref $ update q
    logInfo $ "Subscribing " ++ show key ++ " to " ++ show (toList $ S.tags sub)
    Table idx _ <- readIORef ref
    print idx
    return $! q
  where
    update q (Table idx qs) = Table
        (Index.insert key (toList $ S.tags sub) idx)
        (Queues.insert key q qs)

unsubscribe :: Key a => a -> Routes a -> IO ()
unsubscribe key (Routes ref) = modifyIORef_ ref delete
  where
    delete (Table idx qs) = Table
        (Index.delete key idx)
        (Queues.delete key qs)

notify :: Key a => Event -> Routes a -> IO ()
notify evt (Routes ref) = do
    (Table idx qs) <- readIORef ref

    let tags = T.Tag "*" : toList (E.tags evt)
        keys = concatMap (`Index.inverse` idx) tags

    print tags
    print keys

    mapM_ (push . (`Queues.lookup` qs)) keys
  where
    push (Just q) = liftIO . atomically $ writeTQueue q evt
    push Nothing  = return ()

modifyIORef_ :: IORef a -> (a -> a) -> IO ()
modifyIORef_ ref f = atomicModifyIORef' ref $ \x -> (f x, ())
