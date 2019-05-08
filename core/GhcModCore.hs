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

  -- * Cradle
  , Cradle(..)
  , cradle
  , findCradle'
  -- * Options
  , Options(..)
  , options
  , defaultOptions
  -- * Logging
  , GmLogLevel(..)
  , GmLog(..)
  -- -- * Types
  , GmEnv(..)
  , GmlT(..)
  , GmOut(..)
  , GhcModEnv(..)
  , MonadIO(..)
  , gmlGetSession
  , gmlSetSession
  , GhcModError(..)
  -- * Monad Types
  , GhcModT
  , IOish
  -- * Monad utilities
  , runGhcModT
  -- * Output
  , OutputOpts(..)
  -- * FileMapping
  , makeAbsolute'
  , loadMappedFileSource
  , getMMappedFiles
  , mkRevRedirMapFunc
  , withMappedFile
  -- * HIE integration utilities
  , getModulesGhc'

  , Gap.GhcPs
  , Gap.GhcRn
  , Gap.GhcTc

  ) where

import GhcMod.Cradle ( findCradle')
import GhcMod.DynFlags ( withDynFlags )
import GhcMod.Error ( gcatches, GHandler(..), ghcExceptionDoc )
import GhcMod.FileMapping ( loadMappedFileSource )

import GhcMod.Logging ( renderGm )
import GhcMod.Monad ( GmState(..), cradle, options, GmLog(..), GmEnv(..), GmlT(..), GmOut(..), gmlGetSession, gmlSetSession, GhcModT, runGhcModT, getMMappedFiles )
import GhcMod.ModuleLoader ( getModulesGhc' )
import GhcMod.Target ( cabalResolvedComponents )
import GhcMod.Types ( GmGhcSession(..), GhcModState(..), GmModuleGraph(..),gmcHomeModuleGraph, Cradle(..), Options(..), defaultOptions, GmLogLevel(..), GhcModEnv(..),MonadIO(..), GhcModError(..), IOish, OutputOpts(..)
                    , GhcModError(..) )
import GhcMod.Utils ( makeAbsolute', mkRevRedirMapFunc
                    , withMappedFile)
import qualified GhcMod.Gap as Gap ( mkErrStyle', GhcPs, GhcRn, GhcTc )
