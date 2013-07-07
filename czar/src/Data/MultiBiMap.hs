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

module Data.MultiBiMap
    -- * Opaque Data Type
    ( MultiBiMap

    -- * Constructor
    , empty

    -- * Operations
    , insert
    , delete
    -- , direct
    , inverse
    ) where

import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set
import           Data.Hashable
import           Data.Maybe
import           Prelude             hiding (foldl', lookup)
import           System.IO.Unsafe

type Constraints a b = (Show a, Eq a, Hashable a, Show b, Eq b, Hashable b)

data MultiBiMap a b where
    Map :: Constraints a b
        => HashMap a (HashSet b)
        -> HashMap b (HashSet a)
        -> MultiBiMap a b

deriving instance (Show a, Show b) => Show (MultiBiMap a b)

empty :: Constraints a b => MultiBiMap a b
empty = Map Map.empty Map.empty

insert :: Constraints a b => a -> [b] -> MultiBiMap a b -> MultiBiMap a b
insert k vs (Map dir inv) = Map
    (Map.insertWith Set.union k (Set.fromList vs) dir)
    (foldl' update inv vs)
  where
    update m v = Map.insertWith Set.union v (Set.singleton k) m

delete :: Constraints a b => a -> MultiBiMap a b -> MultiBiMap a b
delete k m@(Map dir inv) = Map
    (Map.delete k dir)
    (foldl' adjust inv $ lookup k dir)
  where
    adjust m v = maybe m
        (\s -> let set = Set.delete k s
              in if Set.null set
                  then Map.delete v m
                  else Map.insert v set m)
        (Map.lookup v m)

direct :: Constraints a b => [a] -> MultiBiMap a b -> [b]
direct ks (Map dir _) = toList $ lookupAll ks dir

inverse :: Constraints a b => [b] -> MultiBiMap a b -> [a]
inverse vs (Map _ inv) = toList $ lookupAll vs inv

--
-- Internal
--

lookupAll :: Constraints a b => [a] -> HashMap a (HashSet b) -> HashSet b
lookupAll ks m = Set.unions $ map (flip lookup m) ks

lookup :: Constraints a b => a -> HashMap a (HashSet b) -> HashSet b
lookup k m = fromMaybe Set.empty $ Map.lookup k m
