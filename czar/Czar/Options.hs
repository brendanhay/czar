{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances               #-}
{-# LANGUAGE TypeSynonymInstances            #-}
{-# LANGUAGE ScopedTypeVariables               #-}

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
    -- * Class to define common options
    ( CommonOptions(..)

    , runProgram

    -- * Option Constructors
    , debugSwitch
    , addressOption
    , secondsOption
    , stringOption
    , stringsOption

    -- * Re-exported Modules
    , module Opts
    ) where

import           Control.Applicative
import           Control.Error                        hiding (err)
import           Czar.Log
import           Czar.Types
import           Data.Monoid
import           Options.Applicative                  as Opts
import           Options.Applicative.Builder.Internal
import           Options.Applicative.Types            as Opts
import qualified Text.ParserCombinators.Parsec        as P

class CommonOptions a where
    debug :: a -> Bool

runProgram :: CommonOptions a => ParserInfo a -> (a -> IO b) -> IO b
runProgram parser io = do
   opts <- execParser parser
   setLogging $ debug opts
   io opts

debugSwitch :: Parser Bool
debugSwitch = switch (long "debug" <> help "Log debug output")

addressOption :: String -> Address -> String -> Parser Address
addressOption = create "ADDR" (reader (fmapL ErrorMsg . parseAddress))

secondsOption :: String -> Seconds -> String -> Parser Seconds
secondsOption = create "SECONDS" (reader (fmap fromInteger . auto))

stringOption :: String -> String -> String -> Parser String
stringOption = create "STR" (reader auto)

stringsOption :: String -> [String] -> String -> Parser [String]
stringsOption = create "[STR,STR,...]" (reader parser)
  where
    parser :: String -> Either ParseError [String]
    parser s = err `fmapL` P.parse list "" s
      where
        list = P.char '['
            *> P.sepBy1 (P.spaces *> P.many1 (P.noneOf "[]," <* P.spaces)) (P.char ',')
            <* P.char ']'

        err = const . ErrorMsg $ "Unable to parse `" ++ s
            ++ "', please ensure it is of the format: [STR, STR, ...]"

--
-- Internal
--

create :: String -> Mod OptionFields a -> String -> a -> String -> Parser a
create var extra key val text =
    nullOption $ metavar var <> long key <> help text <> value val <> extra

