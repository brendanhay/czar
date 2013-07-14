{-# LANGUAGE CPP #-}

-- |
-- Module      : System.Info.Health
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module System.Info.Disk where

#ifdef darwin_HOST_OS
import System.Info.Disk.OSX
#else
import System.Info.Disk.Linux
#endif
