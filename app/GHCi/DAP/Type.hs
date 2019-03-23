module GHCi.DAP.Type where

import qualified GHC as G
import RtClosureInspect

import qualified Data.Map as M
import Control.Concurrent
import HscTypes

import qualified Haskell.DAP as D


-- |
--
type EvalString = String


-- |
--
type ModuleName = String


-- |
--
type MVarDAPContext = MVar DAPContext


-- |
--
data LogLevel = ErrorLogLevel | WarnLogLevel | InfoLogLevel | DebugLogLevel
  deriving (Eq, Ord)

instance Show LogLevel where
  show ErrorLogLevel = "[DAP][ERROR]"
  show WarnLogLevel  = "[DAP][WARN]"
  show InfoLogLevel  = "[DAP][INFO]"
  show DebugLogLevel = "[DAP][DEBUG]"

-- |
--
data SourceBreakpointInfo = SourceBreakpointInfo {
    modNameSourceBreakpointInfo :: ModuleName
  , srcBPSourceBreakpointInfo   :: D.SourceBreakpoint
  , hitCntSourceBreakpointInfo  :: Int
  } deriving (Show, Read, Eq)


-- |
--
data DAPContext = DAPContext {
    variableReferenceMapDAPContext :: M.Map Int (Term, EvalString)
  , bindingDAPContext              :: [G.TyThing]
  , bindingGlobalDAPContext        :: [G.TyThing]
  , frameIdDAPContext              :: Int
  , srcBPsDAPContext               :: M.Map Int SourceBreakpointInfo
  , funcBPsDAPContext              :: M.Map Int (D.FunctionBreakpoint, Int)
  , runStmtDeclExceptionDAPContext :: Maybe SourceError
  , logLevelDAPContext             :: LogLevel
  , stackTraceResultDAPContext     :: Maybe (G.Resume, [G.History])
  , bindingNamesDAPContext         :: [G.Name]
  , isInspectVariableDAPContext    :: Bool
  , continueExecResultDAPContext   :: Maybe G.ExecResult
  }

  
-- |
--
defaultDAPContext :: DAPContext
defaultDAPContext = DAPContext {
    variableReferenceMapDAPContext = M.fromList []
  , bindingDAPContext = []
  , bindingGlobalDAPContext = []
  , frameIdDAPContext = 0
  , srcBPsDAPContext  = M.fromList []
  , funcBPsDAPContext = M.fromList []
  , runStmtDeclExceptionDAPContext = Nothing
  , logLevelDAPContext = InfoLogLevel
  , stackTraceResultDAPContext = Nothing
  , bindingNamesDAPContext = []
  , isInspectVariableDAPContext = True
  , continueExecResultDAPContext = Nothing
  }

  