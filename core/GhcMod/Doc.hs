module GhcMod.Doc
  (
    showPage
  , showOneLine
  , getStyle
  ) where

import GHC
import GhcMod.Gap (withStyle, showDocWith)
import Outputable
import DynFlags
import Pretty (Mode(..))

showPage :: DynFlags -> PprStyle -> SDoc -> String
showPage dflag style = showDocWith dflag PageMode . withStyle dflag style

showOneLine :: DynFlags -> PprStyle -> SDoc -> String
showOneLine dflag style = showDocWith dflag OneLineMode . withStyle dflag style

getStyle :: GhcMonad m => m PprStyle
getStyle = do
    unqual <- getPrintUnqual
    dflags <- getDynFlags
    return $ mkUserStyle dflags unqual AllTheWay

-- styleUnqualified :: DynFlags -> PprStyle
-- styleUnqualified dflags =
-- #if __GLASGOW_HASKELL__ >= 802
--     mkUserStyle dflags neverQualify AllTheWay
-- #else
--     mkUserStyle neverQualify AllTheWay
-- #endif
