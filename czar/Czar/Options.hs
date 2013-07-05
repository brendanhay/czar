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

    -- * Re-exported Modules
    , module Int
    , module Opts
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Czar.Log
import Czar.Options.Internal  as Int
import Data.Version
import Language.Haskell.TH
import Options                as Opts hiding (stringOption, stringsOption)
import Paths_czar             (version)

defineOptions "Common" $ do
    boolOption "optDebug" "debug" False
        "Log debug output"

    boolOption "optVersion" "version" False
        "Show version information"

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
runProgram action = runCommand $ \opts _ -> do
    let Common{..} = getCommon opts

    when optVersion . liftIO $ do
        putStrLn $ showVersion version

    setLogging optDebug >> action opts
