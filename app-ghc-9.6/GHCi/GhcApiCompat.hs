module GHCi.GhcApiCompat (
    module GHC.Runtime.Heap.Inspect
  , module GHC.Unit
  , module GHC.Driver.Monad
  , module GHC.Unit.Module
  , module GHC.Utils.Outputable
  , module GHC.Utils.Exception
  , module GHC.Runtime.Debugger
  , module GHC.Data.FastString
  , module GHC.Runtime.Eval.Types
  , module GHC.Core.DataCon
  , module GHC.Types.TyThing.Ppr
  , module GHC.Driver.Session
  , module GHC.Types.Name.Reader
  , module GHC.Types.SourceError
  , module GHC.Driver.Ppr
  , module GHC.Types.TyThing
  , module GHC.Unit.Module.ModSummary
  , module GHC.Driver.Env.Types
  , module GHC.Runtime.Context
  , module GHC.Types.BreakInfo
  , module GHC.Types.FieldLabel
  , CMC.catch
  , pprTypeForUser
  , ic_rn_gbl_env
  , parseName
  , fls2fs
) where

import GHC.Runtime.Heap.Inspect
import GHC.Unit
import GHC.Driver.Monad
import GHC.Unit.Module
import GHC.Utils.Outputable
import GHC.Utils.Exception hiding (catch)
import GHC.Runtime.Debugger
import GHC.Data.FastString
import GHC.Runtime.Eval.Types
import GHC.Core.DataCon
import GHC.Types.TyThing.Ppr
import GHC.Driver.Session
import GHC.Types.Name.Reader
import GHC.Types.SourceError
import GHC.Driver.Ppr
import GHC.Types.TyThing
import GHC.Unit.Module.ModSummary
import GHC.Driver.Env.Types
import GHC.Runtime.Context
import GHC.Types.BreakInfo
import GHC.Core.TyCo.Ppr
import GHC.Core.Type
import GHC.Types.FieldLabel
import GHC.Plugins
import qualified Control.Monad.Catch as CMC
import qualified GHC.Runtime.Eval as E
import qualified Data.List.NonEmpty as N
import Language.Haskell.Syntax.Basic

-- |
--
--
pprTypeForUser :: Type -> SDoc
pprTypeForUser = pprSigmaType

-- |
--
--
ic_rn_gbl_env :: InteractiveContext -> GlobalRdrEnv
ic_rn_gbl_env = igre_env . ic_gre_cache

-- |
--
--
parseName :: GhcMonad m => String -> m [Name]
parseName s = do
  nel <- E.parseName s
  return $ N.toList nel


-- |
--
--
fls2fs :: FieldLabelString -> FastString
fls2fs (FieldLabelString v) = v
