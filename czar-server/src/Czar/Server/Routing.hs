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
import           Czar.Protocol                       (Event, Subscription, Utf8)
import qualified Czar.Protocol                       as P
import           Czar.Threshold
import           Data.ByteString.Lazy                (ByteString)
import           Data.Foldable
import           Data.HashMap.Strict                 (HashMap)
import qualified Data.HashMap.Strict                 as Queues
import           Data.Hashable
import           Data.IORef
import           Data.MultiBiMap                     (MultiBiMap)
import qualified Data.MultiBiMap                     as Index
import           Prelude                             hiding (concatMap, mapM_, elem)

type Key a = (Eq a, Hashable a, Show a)

data Table a = Table !(MultiBiMap a Utf8) !(HashMap a (TQueue Event))

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
        (Index.insert key match idx)
        (Queues.insert key q qs)

    match | "*" `elem` tags = ["*"]
          | otherwise       = tags

    tags = toList $ S.tags sub

unsubscribe :: (MonadIO m, Key a) => a -> Routes a -> m ()
unsubscribe key (Routes ref) = modifyIORef_ ref delete
  where
    delete (Table idx qs) = Table
        (Index.delete key idx)
        (Queues.delete key qs)

notify :: (MonadIO m, Key a) => Event -> Routes a -> m ()
notify e (Routes ref) = do
    (Table idx qs) <- liftIO $ readIORef ref

    let evt  = annotate e
        tags = "*" : toList (E.tags evt)
        keys = Index.inverse tags idx

    logDebug $ "sending tags " ++ show tags ++ " to " ++ show keys

    mapM_ (push qs evt) keys
  where
    push qs evt k = maybe
       (return ())
       (liftIO . atomically . flip writeTQueue evt)
       (Queues.lookup k qs)

--
-- Internal
--

modifyIORef_ :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyIORef_ ref f = liftIO $ atomicModifyIORef' ref $ \x -> (f x, ())

