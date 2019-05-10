-- ghc-mod: Happy Haskell Hacking
-- Copyright (C) 2015  Daniel Gröber <dxld ÄT darkboxed DOT org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GhcMod.Logging (
    text
  , (<+>)
  , ($$)
  , (<+>:)
  , parens
  , strDoc
  , showToDoc
  , nest
  --
  , renderGm
  , gmLog
  , gmVomit
  , gmSetLogLevel
  , gmAppendLogQuiet

  ) where

-- import Control.Applicative hiding (empty)
import Control.Monad
-- import Control.Monad.Trans.Class
import Data.List
import Data.Char
import Data.Monoid
import Data.Maybe
import System.IO
import System.FilePath
import Prelude

import Pretty hiding (style, (<>))

import GhcMod.Monad.Types
import GhcMod.Types
import GhcMod.Pretty
import GhcMod.Output

gmSetLogLevel :: GmLog m => GmLogLevel -> m ()
gmSetLogLevel level =
    gmlJournal $ GhcModLog (Just level) (Last Nothing) []

-- |
-- >>> Just GmDebug <= Nothing
-- False
-- >>> Just GmException <= Just GmDebug
-- True
-- >>> Just GmDebug <= Just GmException
-- False
gmLog :: (MonadIO m, GmLog m, GmOut m) => GmLogLevel -> String -> Doc -> m ()
gmLog level loc' doc = do
  GhcModLog { gmLogLevel = mlevel' } <- gmlHistory
  level' <- case mlevel' of
    Nothing -> error "mempty value for GhcModLog must use a Just value"
    Just l -> return l

  let loc | loc' == "" = empty
          | otherwise = text loc' <+>: empty
      msgDoc = sep [loc, doc]
      msg = dropWhileEnd isSpace $ renderGm $ gmLogLevelDoc level <+>: msgDoc

  when (level <= level') $ gmErrStrLn msg
  gmLogQuiet level loc' doc

gmLogQuiet :: GmLog m => GmLogLevel -> String -> Doc -> m ()
gmLogQuiet level loc doc =
  gmlJournal (GhcModLog Nothing (Last Nothing) [(level, loc, doc)])

gmAppendLogQuiet :: GmLog m => GhcModLog -> m ()
gmAppendLogQuiet GhcModLog { gmLogMessages } =
    forM_ gmLogMessages $ \(level, loc, doc) -> gmLogQuiet level loc doc

gmVomit :: (MonadIO m, GmLog m, GmOut m, GmEnv m) => String -> Doc -> String -> m ()
gmVomit filename doc content = do
  gmLog GmVomit "" $ doc <+>: text content

  GhcModLog { gmLogVomitDump = Last mdump }
      <- gmlHistory

  dir <- cradleTempDir `liftM` cradle
  when (fromMaybe False mdump) $
       liftIO $ writeFile (dir </> filename) content
