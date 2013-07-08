{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE QuasiQuotes                 #-}


-- |
-- Module      : System.Info.Health.OSX
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module System.Info.Health.OSX
    ( getHealth
    ) where

import           Control.Applicative
import           Control.Error
import           Data.Char
import           Data.List
import           System.ShQQ
import           Text.Parsec.Language                (emptyDef)
import qualified Text.ParserCombinators.Parsec       as P
import           Text.ParserCombinators.Parsec       hiding ((<|>), try, string, char)
import qualified Text.ParserCombinators.Parsec.Token as P
import           System.Info.Types

getHealth :: EitherT String IO Health
getHealth = do
    (cores, top) <- scriptIO $ (,)
        <$> [sh| sysctl -n hw.ncpu |]
        <*> [sh| top -l 1 | grep -i "^\\(cpu\\|physmem\\|load\\)" |]
    n <- readMay cores ?? ("Unable to parse number of cores: " ++ cores)
    hoistEither $! show `fmapL` parse (parseHealth n) "" top

--
-- Internal
--

parseHealth :: Integer -> GenParser Char s Health
parseHealth n = Health n
    <$> parseLoad n
    <*> parseUtil
    <*> parseMemory

-- Load Avg: 0.93, 0.99, 1.03
parseLoad :: Integer -> GenParser Char s Double
parseLoad n = do
    string "load avg:"
    double <* P.char ','
    double <* P.char ','
    (/ fromIntegral n) <$> (double <* P.spaces)

-- CPU usage: 2.75% user, 11.72% sys, 85.51% idle
parseUtil :: GenParser Char s Double
parseUtil = do
    string "cpu usage:"
    double <* string "% user,"
    double <* string "% sys,"
    val <- double <* string "% idle"
    return $! 1 - (val / 100)

-- PhysMem: 2484M wired, 3435M active, 586M inactive, 6505M used, 9868M free.
parseMemory :: GenParser Char s Double
parseMemory = do
    string "physmem:"
    wired    <- value <* string "wired,"
    active   <- value <* string "active,"
    inactive <- value <* string "inactive,"
    used     <- value <* string "used,"
    free     <- value <* string "free"
    return $! (wired + active + used) / (wired + active + used + inactive + free)
  where
    value = do
        val <- fromIntegral <$> integer
        typ <- oneOf "BKMGT" <* P.spaces
        return $! val * (1024 ** maybe 0 fromIntegral (typ `elemIndex` "BKMGT"))

double :: GenParser Char s Double
double = P.spaces *> (P.float $ P.makeTokenParser emptyDef)

integer :: GenParser Char s Integer
integer = P.spaces *> (P.integer $ P.makeTokenParser emptyDef)

char :: Char -> GenParser Char s Char
char c = P.char (toLower c) <|> P.char (toUpper c)

string :: String -> GenParser Char s String
string s = (P.try (mapM char s) <?> "\"" ++ s ++ "\"") <* P.spaces
