-- | The ghc-mod library.

module GhcMod (
    info
  , splits'
  , SplitResult(..)
  ) where

import GhcMod.Exe.CaseSplit ( splits', SplitResult(..) )
import GhcMod.Exe.Info ( info )
