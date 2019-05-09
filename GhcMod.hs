-- | The ghc-mod library.

module GhcMod (
  -- * Cradle
  --   Cradle(..)
  -- , Project(..)
  -- -- , findCradle
  -- -- * Options
  -- , Options(..)
  -- , LineSeparator(..)
  -- , OutputStyle(..)
  -- , FileMapping(..)
  -- , defaultOptions
  -- -- * Logging
  -- , GmLogLevel
  -- -- , increaseLogLevel
  -- -- , decreaseLogLevel
  -- , gmSetLogLevel
  -- , gmLog
  -- -- * Types
  -- , ModuleString
  -- , Expression(..)
  -- , GhcPkgDb
  -- , Symbol
  -- , SymbolDb
  -- , GhcModError(..)
  -- -- * Monad Types
  -- , GhcModT
  -- , IOish
  -- -- * Monad utilities
  -- , runGhcModT
  -- , withOptions
  -- , dropSession
  -- -- * 'GhcMod' utilities
  -- , boot
  -- , browse
  -- , check
  -- , checkSyntax
  -- , debugInfo
  -- , componentInfo
  -- , expandTemplate
    info
  , lint
  ,splits'
  ,SplitResult(..)
  -- , pkgDoc
  -- , rootInfo
  -- , types
  -- , test
  -- , splits
  -- , sig
  -- , refine
  -- -- , auto
  -- , modules
  -- , languages
  -- , flags
  -- , findSymbol
  -- , lookupSymbol
  -- , dumpSymbol
  -- -- * SymbolDb
  -- , loadSymbolDb
  -- , isOutdated
  -- -- * Output
  -- , gmPutStr
  -- , gmErrStr
  -- , gmPutStrLn
  -- , gmErrStrLn
  -- -- * FileMapping
  -- -- , loadMappedFile
  -- , loadMappedFileSource
  -- -- , unloadMappedFile
  ) where

import GhcMod.Exe.CaseSplit
import GhcMod.Exe.Info
import GhcMod.Exe.Lint
