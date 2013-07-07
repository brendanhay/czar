{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell      #-}

-- |
-- Module      : Czar.Options.Internal
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Czar.Options.Internal
    -- * Option Constructors
    ( addressOption
    , secondsOption
    , stringOption
    , stringsOption
    , maybeStringOption
    , maybeDoubleOption
    ) where

import           Control.Monad.IO.Class
import           Czar.Log
import           Czar.Types
import           Data.List              (intercalate)
import           Language.Haskell.TH
import qualified Options                as O
import           Options                hiding (stringOption, stringsOption)
import           Options.OptionTypes

addressOption :: String -> String -> Address -> String -> OptionsM ()
addressOption = createOption
    (OptionType (ConT ''Address) False parseAddress [| parseAddress |])

secondsOption :: String -> String -> Seconds -> String -> OptionsM ()
secondsOption = createOption
    (OptionType (ConT ''Seconds) False parseSeconds [| parseSeconds |])

stringOption :: String -> String -> String -> String -> OptionsM ()
stringOption name flag def desc =
    O.stringOption name flag def $ desc ++ defaultText def

stringsOption :: String -> String -> [String] -> String -> OptionsM ()
stringsOption name flag def desc =
    O.stringsOption name flag def $ desc ++ defaultText (intercalate ", " def)

maybeStringOption :: String -> String -> String -> OptionsM ()
maybeStringOption name flag =
    createOption (optionTypeMaybe optionTypeString) name flag Nothing

maybeDoubleOption :: String -> String -> String -> OptionsM ()
maybeDoubleOption name flag =
    createOption (optionTypeMaybe optionTypeDouble) name flag Nothing

--
-- Internal
--

createOption :: Show a => OptionType a -> String -> String -> a -> String -> OptionsM ()
createOption type' name flag def desc =
    option name $ \o -> o
        { optionLongFlags   = [flag]
        , optionDefault     = show def
        , optionType        = type'
        , optionDescription = desc ++ defaultText (show def)
        }

defaultText :: String -> String
defaultText s
    | s == "Nothing" = " (default: none)"
    | null s        = " (default: none)"
    | otherwise     = " (default: " ++ s ++ ")"
