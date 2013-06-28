{-# LANGUAGE DataKinds,
             TypeOperators,
             KindSignatures,
             GADTs,
             StandaloneDeriving,
             TypeFamilies,
             ConstraintKinds,
             UndecidableInstances,
             PolyKinds,
             Rank2Types #-}

-- |
-- Module      : Data.FSM
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.FSM where

import GHC.Exts

-- A hack to turn the type-level 'elem' defined below into a GHC
-- 'Constraint', which allows for nicer syntax
type family Elem (x :: k) (xs :: [k]) :: Constraint
type instance Elem x xs = (Elem' x xs ~ ValidMessageForState)

data AcceptableMessage
    = ValidMessageForState
    | InvalidMessageForState

-- A type family to encode 'elem' on type-level lists
type family Elem' (x :: k) (xs :: [k]) :: AcceptableMessage
type instance where
    Elem' x '[] = InvalidMessageForState
    Elem' x (x ': xs) = ValidMessageForState
    Elem' x (y ': xs) = Elem' x xs

-- Types of messages which can be pushed through the state
data Message1 = Message1 Int
data Message2 = Message2 Int

-- The state data type parametrised over a list of types.
data State :: [*] -> * where
      State :: Int -> State m

deriving instance Show (State m)

type Handler m n = Elem m ms => State ms -> m -> State n

