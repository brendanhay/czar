{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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
    ( commonOptions
    , runProgram
    , command

    -- * Re-exported Modules
    , module Int
    , module Opts
    ) where

import Control.Applicative
import Control.Monad.IO.Class
import Czar.Log
import Czar.Options.Internal  as Int
import Language.Haskell.TH
import Options                as Opts hiding (stringOption, stringsOption, integerOption)

defineOptions "Common" $ do
    boolOption "optDebug" "debug" False
        "Log debug output"

class CommonOptions a where
    getCommon :: a -> Common

commonOptions :: String -> OptionsM () -> Q [Dec]
commonOptions name rest = (decl :) <$> opts
  where
    decl = InstanceD [] (AppT (ConT ''CommonOptions) (ConT $ mkName name))
        [ValD (VarP 'getCommon) (NormalB . VarE $ mkName "optCommon") []]

    opts = defineOptions name $
        options "optCommon" (importedOptions :: ImportedOptions Common) >> rest

runProgram :: (MonadIO m, Options a, CommonOptions a) => (a -> m b) -> m b
runProgram action = runCommand $ \opts _ -> runCommon opts >> action opts

command :: (MonadIO m, Options a, Options b, CommonOptions a)
           => String
           -> (b -> m c)
           -> Subcommand a (m c)
command name action = subcommand name $ \a b _ -> runCommon a >> action b

--
-- Internal
--

runCommon :: (MonadIO m, Options a, CommonOptions a) => a -> m ()
runCommon opts = do
    let Common{..} = getCommon opts
    setLogging optDebug
