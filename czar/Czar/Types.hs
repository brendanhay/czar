{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Czar.Types
    ( Seconds
    , parseSeconds
    , toInt

    , Address(..)
    , parseAddress

    , parseThreshold
    ) where

import           Control.Applicative
import           Control.Error
import           Czar.Internal.Protocol.Threshold
import           Data.Hashable
import           Data.String
import           Network.Socket
import           Options.OptionTypes
import           System.FilePath
import           System.IO.Unsafe                    (unsafePerformIO)
import           Text.Parsec.Language                (emptyDef)
import qualified Text.ParserCombinators.Parsec       as P
import           Text.ParserCombinators.Parsec       hiding ((<|>), try)
import qualified Text.ParserCombinators.Parsec.Token as P

newtype Seconds = Seconds Int deriving (Eq, Ord, Num)

instance Show Seconds where
    show (Seconds n) = show n

parseSeconds :: String -> Either String Seconds
parseSeconds = fmap fromInteger . parseInteger

toInt :: Seconds -> Int
toInt (Seconds n) = n * 1000000

newtype Address = Address SockAddr

instance Show Address where
    show (Address addr) = case addr of
        SockAddrUnix{..} -> "unix://" ++ show addr
        _                -> "tcp://" ++ show addr

instance Read Address where
    readsPrec _ = either
        (error . ("Failed to parse Address from: " ++))
        (\addr -> [(addr, "")])
        . parseAddress

instance IsString Address where
    fromString = read

parseAddress :: String -> Either String Address
parseAddress = parseE addrWrapper

instance Hashable SockAddr where
    hashWithSalt salt = hashWithSalt salt . show

parseThreshold :: String -> Either String Threshold
parseThreshold = parseE parser
  where
    parser = do
        l <- optionMaybe double
        char ':'
        u <- optionMaybe double
        return $! Threshold l u

--
-- Internal
--

-- parseE :: GenParser Char s a -> String -> Either String a
parseE p str = f `fmapL` parse p "" str
  where
    f = (str ++) . (" " ++) . show

addrWrapper :: GenParser Char s Address
addrWrapper = do
    s <- (string "tcp" P.<|> string "unix") <* string "://"
    Address <$> case s of
        "tcp"  -> addrTcp
        "unix" -> addrUnix
        _      -> fail "Unable to parse addr"

addrTcp :: GenParser Char s SockAddr
addrTcp = do
    h <- unsafePerformIO . inet_addr <$> addrPath
    p <- addrPort
    return $! SockAddrInet p h

addrUnix :: GenParser Char s SockAddr
addrUnix = do
    p <- addrPath
    if isValid p
     then return $! SockAddrUnix p
     else fail $ "Invalid unix:// socket path: " ++ p

addrPort :: GenParser Char s PortNumber
addrPort = f <$> (char ':' *> (read <$> many1 digit) <* eof)
  where
    f x = fromIntegral (x :: Integer)

addrPath :: GenParser Char s String
addrPath = many1 $ noneOf "!@#$%^&*()=\\/|\"',?:"

double :: GenParser Char s Double
double = P.float $ P.makeTokenParser emptyDef
