{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell      #-}

-- |
-- Module      : Czar.Options.Internal
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
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
    , thresholdOption
    , stringOption
    , stringsOption
    , maybeStringOption
    , maybeDoubleOption
    ) where

import           Czar.Internal.Protocol.Threshold
import           Czar.Types
import           Data.List                        (intercalate)
import           Language.Haskell.TH
import qualified Options                          as O
import           Options                          hiding (stringOption, stringsOption)
import           Options.OptionTypes

addressOption :: String -> String -> Address -> String -> OptionsM ()
addressOption = createOption
    (OptionType (ConT ''Address) False parseAddress [| parseAddress |])

secondsOption :: String -> String -> Seconds -> String -> OptionsM ()
secondsOption = createOption
    (OptionType (ConT ''Seconds) False parseSeconds [| parseSeconds |])

thresholdOption :: String -> String -> Threshold -> String -> OptionsM ()
thresholdOption name flag (Threshold l u) desc =
    option name $ \o -> o
        { optionLongFlags   = [flag]
        , optionDefault     = def
        , optionType        = type'
        , optionDescription = desc ++ defaultText def
        }
  where
    type' = OptionType (ConT ''Threshold) False parseThreshold [| parseThreshold |]
    def   = maybe "" show l ++ ":" ++ maybe "" show u

stringOption :: String -> String -> String -> String -> OptionsM ()
stringOption name flag def desc =
    O.stringOption name flag def $ desc ++ defaultText def

stringsOption :: String -> String -> [String] -> String -> OptionsM ()
stringsOption name flag def desc =
    O.stringsOption name flag def $ desc ++ defaultText (intercalate ", " def)

maybeStringOption :: String -> String -> String -> OptionsM ()
maybeStringOption name flag desc =
    option name $ \o -> o
        { optionLongFlags   = [flag]
        , optionType        = optionTypeMaybe optionTypeString
        , optionDescription = desc ++ defaultText ""
        }

maybeDoubleOption :: String -> String -> String -> OptionsM ()
maybeDoubleOption name flag desc =
    option name $ \o -> o
        { optionLongFlags   = [flag]
        , optionType        = optionTypeMaybe optionTypeDouble
        , optionDescription = desc ++ defaultText ""
        }

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
    | null s    = " (default: none)"
    | otherwise = " (default: " ++ s ++ ")"
