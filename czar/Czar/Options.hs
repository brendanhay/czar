{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances               #-}
{-# LANGUAGE TypeSynonymInstances            #-}
{-# LANGUAGE ScopedTypeVariables             #-}
{-# LANGUAGE TemplateHaskell                 #-}

-- |
-- Module      : Czar.Options
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Czar.Options
    -- * Option Declarations
    ( debugOption
    , emissionOption

    -- * Option Constructors
    , addressOption
    , secondsOption

    -- * Re-exported Modules
    , module Opts
    ) where

import Czar.Types
import Language.Haskell.TH
import Options             as Opts
import Options.OptionTypes

debugOption :: OptionsM ()
debugOption = boolOption "optDebug" "debug" False
    "Log debug output"

emissionOption :: OptionsM ()
emissionOption = secondsOption "optEmission" "emit-every" 30
    "Interval between internal metric emissions"

addressOption :: String -> String -> Address -> String -> OptionsM ()
addressOption = createOption
    (OptionType (ConT ''Address) False parseAddress [| parseAddress |])

secondsOption :: String -> String -> Seconds -> String -> OptionsM ()
secondsOption = createOption
    (OptionType (ConT ''Seconds) False parseSeconds [| parseSeconds |])

--
-- Internal
--

createOption :: Show a => OptionType a -> String -> String -> a -> String -> OptionsM ()
createOption type' name flag def desc =
    option name $ \o -> o
        { optionLongFlags   = [flag]
        , optionDefault     = show def
        , optionType        = type'
        , optionDescription = desc
        }

