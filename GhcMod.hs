-- | The ghc-mod library.

module GhcMod (
    info
  , lint
  , splits'
  , SplitResult(..)
  ) where

import GhcMod.Exe.CaseSplit ( splits', SplitResult(..) )
import GhcMod.Exe.Info ( info )
import GhcMod.Exe.Lint ( lint )
