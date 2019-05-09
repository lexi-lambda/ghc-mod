-- | The ghc-mod library.

module GhcModCore (
  -- * Used in hie-plugin-api
    withDynFlags
  , gcatches
  , GHandler(..)
  , ghcExceptionDoc
  , Gap.mkErrStyle'
  , renderGm
  , cabalResolvedComponents
  , GmState(..)
  , GmGhcSession(..)
  , GhcModState(..)
  , GmModuleGraph(..)
  , gmcHomeModuleGraph
  , getModulesGhc'
  , Cradle(..)
  , cradle
  , GhcModT
  , runGhcModT
  , GmlT(..)
  , gmlGetSession
  , gmlSetSession
  , MonadIO(..)
  , GmLogLevel(..)
  , Options(..)
  , options
  , defaultOptions
  , OutputOpts(..)
  , findCradle'
  , GmEnv(..)
  , GmLog(..)
  , GmOut(..)
  , GhcModEnv(..)
  , mkRevRedirMapFunc
  , makeAbsolute'
  , getMMappedFiles
  , IOish
  , GhcModError(..)

  , Gap.GhcPs
  , Gap.GhcRn
  , Gap.GhcTc

-- ---------------------------------------------

  -- * HIE integration utilities

  , loadMappedFileSource
  , withMappedFile
  , Gap.listVisibleModuleNames
  , defaultLintOpts
  , Expression(..)
  , pretty
  , LightGhc(..)
  , runLightGhc
  ) where

import GhcMod.Cradle ( findCradle')
import GhcMod.DynFlags ( withDynFlags )
import GhcMod.Error ( gcatches, GHandler(..), ghcExceptionDoc )
import GhcMod.FileMapping ( loadMappedFileSource )
import GhcMod.Logging ( renderGm )

import GhcMod.Monad ( runGhcModT )
import GhcMod.Monad.Types ( GmState(..), cradle, options, GmLog(..), GmEnv(..), GmlT(..), GmOut(..), gmlGetSession, gmlSetSession, GhcModT, getMMappedFiles, LightGhc(..) )

import GhcMod.ModuleLoader ( getModulesGhc' )
import GhcMod.Target ( cabalResolvedComponents )
import GhcMod.Types ( GmGhcSession(..), GhcModState(..), GmModuleGraph(..),gmcHomeModuleGraph, Cradle(..), Options(..), defaultOptions, GmLogLevel(..), GhcModEnv(..),MonadIO(..), GhcModError(..), IOish, OutputOpts(..)
                    , GhcModError(..), defaultLintOpts, Expression(..) )
import GhcMod.Utils ( makeAbsolute', mkRevRedirMapFunc
                    , withMappedFile)
import qualified GhcMod.Gap as Gap ( mkErrStyle', GhcPs, GhcRn, GhcTc, listVisibleModuleNames )
import GhcMod.LightGhc ( runLightGhc )
import GhcMod.SrcUtils ( pretty )
