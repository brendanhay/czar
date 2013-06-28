{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      : Data.MultiBiMap
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.MultiBiMap (
    -- * Opaque Data Type
      MultiBiMap

    -- * Constructor
    , empty

    -- * Operations
    , insert
    , delete
    , direct
    , inverse
    ) where

import Data.Hashable
import Prelude       hiding (lookup)

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S

type Constraints a b = (Eq a, Hashable a, Eq b, Hashable b)

data MultiBiMap a b where
    Map :: Constraints a b
        => H.HashMap a (S.HashSet b)
        -> H.HashMap b (S.HashSet a)
        -> MultiBiMap a b

-- instance (Show a, Show b) => Show (MultiBiMap a b) where
--     show (Map dir inv) = "MultiBiMap " ++ show dir ++ " " ++ show inv

deriving instance (Show a, Show b) => Show (MultiBiMap a b)

empty :: Constraints a b => MultiBiMap a b
empty = Map H.empty H.empty

insert :: Constraints a b => a -> [b] -> MultiBiMap a b -> MultiBiMap a b
insert k vs (Map dir inv) = Map
    (H.insertWith S.union k (S.fromList vs) dir)
    (foldl update inv vs)
  where
    update m v = H.insertWith S.union v (S.singleton k) m

delete :: Constraints a b => a -> MultiBiMap a b -> MultiBiMap a b
delete k m@(Map dir inv) = Map
    (H.delete k dir)
    (foldl (flip H.delete) inv $ direct k m)

direct :: Constraints a b => a -> MultiBiMap a b -> [b]
direct k (Map dir _) = lookup k dir

inverse :: Constraints a b => b -> MultiBiMap a b -> [a]
inverse v (Map _ inv) = lookup v inv

--
-- Internal
--

lookup :: (Eq a, Hashable a) =>  a -> H.HashMap a (S.HashSet b) -> [b]
lookup k = maybe [] S.toList . H.lookup k
