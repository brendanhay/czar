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

module Czar.Server.Routing
    ( Routes

    -- * Constructor
    , emptyRoutes

    -- * Operations
    , subscribe
    , unsubscribe
    , notify
    ) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import qualified Czar.Internal.Protocol.Event        as E
import qualified Czar.Internal.Protocol.Subscription as S
import           Czar.Log
import           Czar.Protocol                       (Event, Subscription)
import qualified Czar.Protocol                       as P
import           Data.ByteString.Lazy                (ByteString)
import           Data.Foldable                       (toList)
import           Data.HashMap.Strict                 (HashMap)
import qualified Data.HashMap.Strict                 as Queues
import           Data.Hashable
import           Data.IORef
import           Data.MultiBiMap                     (MultiBiMap)
import qualified Data.MultiBiMap                     as Index

type Key a = (Eq a, Hashable a, Show a)

data Table a = Table !(MultiBiMap a ByteString) !(HashMap a (TQueue Event))

newtype Routes a = Routes (IORef (Table a))

emptyRoutes :: (Functor m, MonadIO m, Key a) => m (Routes a)
emptyRoutes = Routes <$> liftIO (newIORef $ Table Index.empty Queues.empty)

subscribe :: (MonadIO m, Key a) => Subscription -> a -> Routes a -> m (TQueue Event)
subscribe sub key (Routes ref) = do
    q <- liftIO newTQueueIO
    modifyIORef_ ref $! update q
    return $! q
  where
    update q (Table idx qs) = Table
        (Index.insert key (map P.utf8 . toList $ S.tags sub) idx)
        (Queues.insert key q qs)

-- FIXME: unsubscribe seems to be removing _all_?

unsubscribe :: (MonadIO m, Key a) => a -> Routes a -> m ()
unsubscribe key (Routes ref) = modifyIORef_ ref delete
  where
    delete (Table idx qs) = Table
        (Index.delete key idx)
        (Queues.delete key qs)

notify :: (MonadIO m, Key a) => Event -> Routes a -> m ()
notify evt (Routes ref) = do
    (Table idx qs) <- liftIO $ readIORef ref

    let tags = "*" : (map P.utf8 . toList $ E.tags evt)
        keys = concatMap (`Index.inverse` idx) tags

    logDebug $ "sending to " ++ show keys

    mapM_ (push . (`Queues.lookup` qs)) keys
  where
    push (Just q) = liftIO . atomically $ writeTQueue q evt
    push Nothing  = return ()

--
-- Internal
--

modifyIORef_ :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyIORef_ ref f = liftIO $ atomicModifyIORef' ref $ \x -> (f x, ())
