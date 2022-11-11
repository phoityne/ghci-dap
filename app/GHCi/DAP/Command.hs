{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module GHCi.DAP.Command where

import qualified GHCi.GhcApiCompat as GAC
import Control.Monad.IO.Class

import qualified GHC as G
import qualified GHC.Data.StringBuffer as SB (lexemeToString, len)
import qualified GHCi.UI as Gi
import qualified GHCi.UI.Monad as Gi hiding (runStmt)

import Control.Concurrent
import Control.Monad
import System.Console.Haskeline
import System.Directory
import qualified Data.Map as M
import qualified Data.List as L

import GHCi.DAP.Type
import GHCi.DAP.Constant
import GHCi.DAP.Utility
import qualified Haskell.DAP as D


-- |
--
dapCommands :: [Gi.Command]
dapCommands = map mkCmd [
    ("dap-launch",                   Gi.keepGoing launchCmd,         noCompletion)
  , ("dap-context-modules",          Gi.keepGoing contextModulesCmd, noCompletion)
  , ("dap-set-breakpoints",          Gi.keepGoing setBpCmd,          noCompletion)
  , ("dap-set-function-breakpoints", Gi.keepGoing setFuncBpsCmd,     noCompletion)
  , ("dap-set-function-breakpoint",  Gi.keepGoing setFuncBpCmd,      noCompletion)
  , ("dap-delete-breakpoint",        Gi.keepGoing delBpCmd,          noCompletion)
  , ("dap-stacktrace",               Gi.keepGoing dapStackTraceCmd,  noCompletion)
  , ("dap-scopes",                   Gi.keepGoing dapScopesCmd,      noCompletion)
  , ("dap-variables",                Gi.keepGoing dapVariablesCmd,   noCompletion)
  , ("dap-evaluate",                 Gi.keepGoing dapEvalCmd,        noCompletion)
  , ("dap-continue",                 Gi.keepGoing dapContinueCmd,    noCompletion)
  , ("dap-next",                     Gi.keepGoing nextCmd,           noCompletion)
  , ("dap-step-in",                  Gi.keepGoing stepInCmd,         noCompletion)
  , ("dap-source",                   Gi.keepGoing sourceCmd,         noCompletion)
  ]
  where
    mkCmd (n,a,c) = Gi.Command {
                    Gi.cmdName = n
                  , Gi.cmdAction = a
                  , Gi.cmdHidden = False
                  , Gi.cmdCompletionFunc = c
                  }



------------------------------------------------------------------------------------------------
--  DAP Command :dap-launch
------------------------------------------------------------------------------------------------
-- |
--
launchCmd :: String -> Gi.GHCi ()
launchCmd argsStr = flip gcatch errHdl $ do
  decodeDAP argsStr
  >>= launchCmd_
  >>= printDAP

-- |
--
launchCmd_ :: D.LaunchRequestArguments
          -> Gi.GHCi (Either String ())
launchCmd_ arg = do
  setLogLevel
  setForceInspect
  return $ Right ()
  where
    -- |
    --
    setLogLevel :: Gi.GHCi ()
    setLogLevel = do
      let lv = case D.logLevelLaunchRequestArguments arg of
                 "EMERGENCY" -> ErrorLogLevel
                 "ALERT"     -> ErrorLogLevel
                 "CRITICAL"  -> ErrorLogLevel
                 "ERROR"     -> ErrorLogLevel
                 "WARNING"   -> WarnLogLevel
                 "NOTICE"    -> WarnLogLevel
                 "INFO"      -> InfoLogLevel
                 "DEBUG"     -> DebugLogLevel
                 _           -> WarnLogLevel

      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      ctx <- liftIO $ takeMVar ctxMVar
      liftIO $ putMVar ctxMVar ctx {logLevelDAPContext = lv}

    -- |
    --
    setForceInspect :: Gi.GHCi ()
    setForceInspect = do
      let isForce = case D.forceInspectLaunchRequestArguments arg of
                      Nothing -> False
                      Just a  -> a

      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      ctx <- liftIO $ takeMVar ctxMVar
      liftIO $ putMVar ctxMVar ctx {isInspectVariableDAPContext = isForce}


------------------------------------------------------------------------------------------------
--  DAP Command :dap-context-modules
------------------------------------------------------------------------------------------------
-- |
--
contextModulesCmd :: String -> Gi.GHCi ()
contextModulesCmd _ = flip gcatch errHdl $ do
  contextModulesCmd_ >>= printDAP

-- |
--
contextModulesCmd_ :: Gi.GHCi (Either String ())
contextModulesCmd_ = do
  modSums <- Gi.getLoadedModules
  let modNames = map GAC.ms_mod_name modSums
      modNameStrs = map G.moduleNameString modNames

  Gi.setContext modNames []
  infoL $ "context modules. " ++ show modNameStrs

  return $ Right ()


------------------------------------------------------------------------------------------------
--  DAP Command :dap-set-breakpoints
------------------------------------------------------------------------------------------------
-- |
--
setBpCmd :: String -> Gi.GHCi ()
setBpCmd argsStr = flip gcatch errHdl $ do
  decodeDAP argsStr
  >>= setBpCmd_
  >>= printDAP

-- |
--
setBpCmd_ :: D.SetBreakpointsRequestArguments
          -> Gi.GHCi (Either String D.SetBreakpointsResponseBody)
setBpCmd_ args =
  deleteBreakpoints >> addBreakpoints

  where
    -- |
    --
    deleteBreakpoints :: Gi.GHCi ()
    deleteBreakpoints = do
      bps <- getDelBPs
      debugL $ "<dapSetBreakpointsCommand> delete src bps " ++ show bps
      mapM_ delBreakpoint bps

    -- |
    --
    addBreakpoints :: Gi.GHCi (Either String D.SetBreakpointsResponseBody)
    addBreakpoints = do
      let srcBPs = D.breakpointsSetBreakpointsRequestArguments args
      addBps <- mapM addBP srcBPs
      updateBpCtx addBps
      return $ Right $ D.SetBreakpointsResponseBody $ map takeBp addBps

    -- |
    --
    getDelBPs :: Gi.GHCi [Int]
    getDelBPs = do
      mod <- getModule
      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      ctx <- liftIO $ takeMVar ctxMVar
      let bpNOs = M.keys $ M.filter ((isModuleMatch mod)) $ srcBPsDAPContext ctx
          newSrcBPs = M.filter (not . (isModuleMatch mod)) $ srcBPsDAPContext ctx

      liftIO $ putMVar ctxMVar $ ctx {srcBPsDAPContext = newSrcBPs}
      return bpNOs

    -- |
    --
    isModuleMatch :: ModuleName -> SourceBreakpointInfo -> Bool
    isModuleMatch mod bpInfo = mod == modNameSourceBreakpointInfo bpInfo

    -- |
    --
    getModule :: Gi.GHCi ModuleName
    getModule = do
      let srcInfo = D.sourceSetBreakpointsRequestArguments args
          srcPath = D.pathSource srcInfo

      modSums <- Gi.getLoadedModules
      let modPaths = map takeModPath modSums

      findModule srcPath modPaths >>= \case
        Just (m, p) -> do
          debugL $ "<getModule> " ++ p ++ " -> " ++ m
          return m

        Nothing -> throwError $ "<getModule> loaded module can not find from path. <" ++ srcPath ++ "> " ++  show modPaths


    -- |
    --
    takeModPath :: GAC.ModSummary -> (ModuleName, FilePath)
    takeModPath ms = (G.moduleNameString (G.ms_mod_name ms), G.ms_hspp_file ms)


    -- |
    --
    addBP :: D.SourceBreakpoint -> Gi.GHCi (ModuleName, D.SourceBreakpoint, D.Breakpoint)
    addBP srcBP = do
      mod <- getModule
      let lineNo   = show $ D.lineSourceBreakpoint srcBP
          colNo    = getColNo $ D.columnSourceBreakpoint srcBP
          argStr   = mod ++ " " ++ lineNo ++ " " ++ colNo

      bp <- addBreakpoint argStr

      return (mod, srcBP, bp)

    -- |
    --
    getColNo :: Maybe Int -> String
    getColNo Nothing  = ""
    getColNo (Just 1) = ""
    getColNo (Just a) = show a

    -- |
    --
    updateBpCtx :: [(ModuleName, D.SourceBreakpoint, D.Breakpoint)] -> Gi.GHCi ()
    updateBpCtx bps = do
      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      ctx <- liftIO $ takeMVar ctxMVar
      let cur = srcBPsDAPContext ctx
          new = M.fromList $ foldr convSrcBps [] bps
      liftIO $ putMVar ctxMVar $ ctx{srcBPsDAPContext = (M.union cur new)}

    -- |
    --
    convSrcBps :: (ModuleName, D.SourceBreakpoint, D.Breakpoint)
               -> [(Int, SourceBreakpointInfo)]
               -> [(Int, SourceBreakpointInfo)]
    convSrcBps (mod, srcBp, bp) acc = case D.idBreakpoint bp of
      Nothing -> acc
      Just no -> (no, SourceBreakpointInfo mod srcBp 0) : acc

    -- |
    --
    takeBp :: (ModuleName, D.SourceBreakpoint, D.Breakpoint) -> D.Breakpoint
    takeBp (_, _, bp) = bp


------------------------------------------------------------------------------------------------
--  DAP Command :dap-set-function-breakpoints
------------------------------------------------------------------------------------------------
-- |
--
setFuncBpsCmd :: String -> Gi.GHCi ()
setFuncBpsCmd argsStr = flip gcatch errHdl $ do
  decodeDAP argsStr
  >>= setFuncBpsCmd_
  >>= printDAP

-- |
--
setFuncBpsCmd_ :: D.SetFunctionBreakpointsRequestArguments
               -> Gi.GHCi (Either String D.SetFunctionBreakpointsResponseBody)
setFuncBpsCmd_ args = deleteBreakpoints >> addBreakpoints
  where

    -- |
    --
    deleteBreakpoints :: Gi.GHCi ()
    deleteBreakpoints = do
      bps <- getDelBPs
      debugL $ "<dapSetFunctionBreakpointsCommand> delete func bps " ++ show bps
      mapM_ delBreakpoint bps

    -- |
    --
    addBreakpoints :: Gi.GHCi (Either String D.SetFunctionBreakpointsResponseBody)
    addBreakpoints = do
      let funcBPs = D.breakpointsSetFunctionBreakpointsRequestArguments args

      addBps <- mapM addBP funcBPs
      updateBpCtx addBps
      return $ Right $ D.SetFunctionBreakpointsResponseBody $ map snd addBps

    -- |
    --
    getDelBPs :: Gi.GHCi [Int]
    getDelBPs = do
      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      ctx <- liftIO $ takeMVar ctxMVar
      let bpNOs = M.keys $ funcBPsDAPContext ctx

      liftIO $ putMVar ctxMVar $ ctx {funcBPsDAPContext = M.fromList []}
      return bpNOs

    -- |
    --
    addBP :: D.FunctionBreakpoint -> Gi.GHCi (D.FunctionBreakpoint, D.Breakpoint)
    addBP funcBP = do
      let argStr = D.nameFunctionBreakpoint funcBP
      bp <- addBreakpoint argStr
      return (funcBP, bp)

    -- |
    --
    updateBpCtx :: [(D.FunctionBreakpoint, D.Breakpoint)] -> Gi.GHCi ()
    updateBpCtx bps = do
      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      ctx <- liftIO $ takeMVar ctxMVar
      let new = foldr getBpNo [] bps
      liftIO $ putMVar ctxMVar $ ctx{funcBPsDAPContext = M.fromList new}

    -- |
    --
    getBpNo :: (D.FunctionBreakpoint, D.Breakpoint)
            -> [(Int, (D.FunctionBreakpoint, Int))]
            -> [(Int, (D.FunctionBreakpoint, Int))]
    getBpNo (funcBP, bp) acc = case D.idBreakpoint bp of
      Nothing -> acc
      Just no -> (no, (funcBP, 0)) : acc


------------------------------------------------------------------------------------------------
--  DAP Command :dap-set-function-breakpoint
------------------------------------------------------------------------------------------------
-- |
--
setFuncBpCmd :: String -> Gi.GHCi ()
setFuncBpCmd argsStr = flip gcatch errHdl $ do
  decodeDAP argsStr
  >>= setFuncBpCmd_
  >>= printDAP

-- |
--
setFuncBpCmd_ :: (FilePath, D.FunctionBreakpoint)
              -> Gi.GHCi (Either String D.Breakpoint)
setFuncBpCmd_ (startup, funcBP) = do
  modName <- getModuleByFile
  let funcName = D.nameFunctionBreakpoint funcBP
      argStr   = modName ++ "." ++ funcName

  bp <- addBreakpoint argStr
  updateBpCtx (funcBP, bp)

  return $ Right bp

  where
    -- |
    --
    getModuleByFile :: Gi.GHCi String
    getModuleByFile = do
      modSums <- Gi.getLoadedModules
      let modPaths = map takeModPath modSums

      findModule startup modPaths >>= \case
        Just (m, p) -> do
          debugL $ "<getModuleByFile> " ++ p ++ " -> " ++ m
          return m

        Nothing -> throwError $ "<getModuleByFile> loaded module can not find from path. <" ++ startup ++ "> " ++  show modPaths

    -- |
    --
    updateBpCtx :: (D.FunctionBreakpoint, D.Breakpoint) -> Gi.GHCi ()
    updateBpCtx (funcBP, bp) = case D.idBreakpoint bp of
      Nothing -> throwError "breakpoint number not found."
      Just no -> do
        ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
        ctx <- liftIO $ takeMVar ctxMVar
        let funcBpMap =  funcBPsDAPContext ctx

        liftIO $ putMVar ctxMVar $ ctx{funcBPsDAPContext = M.insert no (funcBP, 0) funcBpMap}


------------------------------------------------------------------------------------------------
--  DAP Command :dap-delete-breakpoint
------------------------------------------------------------------------------------------------
-- |
--
delBpCmd :: String -> Gi.GHCi ()
delBpCmd argsStr = flip gcatch errHdl $ do
  decodeDAP argsStr
  >>= delBpCmd_
  >>= printDAP

-- |
--
delBpCmd_ :: D.Breakpoint -> Gi.GHCi (Either String ())
delBpCmd_ D.Breakpoint{D.idBreakpoint = Nothing} = throwError "breakpoint number not found."
delBpCmd_ D.Breakpoint{D.idBreakpoint = Just bid} = do
  delBreakpoint bid
  updateBpCtx
  return $ Right ()
  where
    -- |
    --
    updateBpCtx :: Gi.GHCi ()
    updateBpCtx = do
      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      ctx <- liftIO $ takeMVar ctxMVar
      let funcBpMap =  funcBPsDAPContext ctx

      liftIO $ putMVar ctxMVar $ ctx{funcBPsDAPContext = M.delete bid funcBpMap}


------------------------------------------------------------------------------------------------
--  DAP Command :dap-stacktrace
------------------------------------------------------------------------------------------------
-- |
--
dapStackTraceCmd :: String -> Gi.GHCi ()
dapStackTraceCmd argsStr = flip gcatch errHdl $ do
  decodeDAP argsStr
  >>= dapStackTraceCmd_
  >>= printDAP

-- |
--
dapStackTraceCmd_ :: D.StackTraceRequestArguments
                  -> Gi.GHCi (Either String D.StackTraceResponseBody)
dapStackTraceCmd_ _ = do
  clearStackTraceResult

  Gi.historyCmd ""

  getStackTraceResult >>= \case
    Nothing  -> throwError $ "no stacktrace found."
    Just res -> withResult res

  where

    -- |
    --
    clearStackTraceResult :: Gi.GHCi ()
    clearStackTraceResult = do
      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      ctx <- liftIO $ takeMVar ctxMVar
      liftIO $ putMVar ctxMVar ctx {stackTraceResultDAPContext = Nothing}

    -- |
    --
    getStackTraceResult :: Gi.GHCi (Maybe (G.Resume, [G.History]))
    getStackTraceResult = do
      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      ctx <- liftIO $ readMVar ctxMVar
      return $ stackTraceResultDAPContext ctx

    -- |
    --
    withResult :: (G.Resume, [G.History]) -> Gi.GHCi (Either String D.StackTraceResponseBody)
    withResult (r, hs) = do
      hists <- mapM resumeHist2stackFrame hs
      let traces = if isExceptionResume r
                     then hists
                     else resume2stackframe r : hists
      let traceWithId = setFrameIdx 0 traces
      traceWithId2 <- liftIO $ mapM convertToAbs traceWithId

      return $ Right D.defaultStackTraceResponseBody {
          D.stackFramesStackTraceResponseBody = traceWithId2
        , D.totalFramesStackTraceResponseBody = length traceWithId2
        }

    -- |
    --
    convertToAbs :: D.StackFrame -> IO D.StackFrame
    convertToAbs sf@D.StackFrame{D.sourceStackFrame=ssf}
      | D.pathSource ssf == "UnhelpfulSpan" = return sf
      | otherwise = do
          ps <- canonicalizePath $ D.pathSource ssf
          return sf{D.sourceStackFrame=ssf{D.pathSource = ps}}

    -- |
    --
    resumeHist2stackFrame :: G.History -> Gi.GHCi D.StackFrame
    resumeHist2stackFrame hist= do
      span <- G.getHistorySpan hist
      let name = L.intercalate ":" (G.historyEnclosingDecls hist)

      return $ genStackFrame span name

    -- |
    --
    resume2stackframe :: G.Resume -> D.StackFrame
    resume2stackframe r = genStackFrame (G.resumeSpan r) (getStackFrameTitle r)

    -- |
    --
    setFrameIdx :: Int -> [D.StackFrame] -> [D.StackFrame]
    setFrameIdx _ [] = []
    setFrameIdx idx (x:xs) = x{D.idStackFrame = idx} : setFrameIdx (idx+1) xs

    -- |
    --
    getStackFrameTitle :: G.Resume -> String
    getStackFrameTitle r =  maybe "unknown" (G.moduleNameString  . G.moduleName . G.breakInfo_module) (G.resumeBreakInfo r)
                         ++ "."
                         ++ G.resumeDecl r

    -- |
    --
    genStackFrame :: G.SrcSpan -> String -> D.StackFrame
#if __GLASGOW_HASKELL__ >= 900
    genStackFrame (G.RealSrcSpan dat _) name = D.defaultStackFrame {
#else
    genStackFrame (G.RealSrcSpan dat) name = D.defaultStackFrame {
#endif
        D.idStackFrame        = 0
      , D.nameStackFrame      = name
      , D.sourceStackFrame    = D.defaultSource {D.pathSource = (GAC.unpackFS . G.srcSpanFile) dat}
      , D.lineStackFrame      = G.srcSpanStartLine dat
      , D.columnStackFrame    = G.srcSpanStartCol dat
      , D.endLineStackFrame   = G.srcSpanEndLine dat
      , D.endColumnStackFrame = G.srcSpanEndCol dat
      }
    genStackFrame (G.UnhelpfulSpan _) name = D.defaultStackFrame {
        D.idStackFrame        = 0
      , D.nameStackFrame      = name
      , D.sourceStackFrame    = D.defaultSource {D.pathSource = "UnhelpfulSpan"}
      , D.lineStackFrame      = 0
      , D.columnStackFrame    = 0
      , D.endLineStackFrame   = 0
      , D.endColumnStackFrame = 0
      }


------------------------------------------------------------------------------------------------
--  DAP Command :dap-scopes
------------------------------------------------------------------------------------------------
-- |
--
dapScopesCmd :: String -> Gi.GHCi ()
dapScopesCmd argsStr = flip gcatch errHdl $ do
  decodeDAP argsStr
  >>= dapScopesCmd_
  >>= printDAP

-- |
--
dapScopesCmd_ :: D.ScopesRequestArguments
              -> Gi.GHCi (Either String D.ScopesResponseBody)
dapScopesCmd_ args = moveScope >> makeResponse

  where
    -- |
    --
    moveScope :: Gi.GHCi ()
    moveScope = do
      let curIdx = D.frameIdScopesRequestArguments args
      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      oldIdx  <- liftIO $ frameIdDAPContext <$> readMVar ctxMVar
      let moveIdx = curIdx - oldIdx

      tyThings <- withMoveIdx moveIdx
      gobalTT  <- getGlobalBindings

      ctx <- liftIO $ takeMVar ctxMVar
      liftIO $ putMVar ctxMVar ctx {
          variableReferenceMapDAPContext = M.empty
        , bindingDAPContext              = tyThings
        , bindingGlobalDAPContext        = gobalTT
        , frameIdDAPContext              = curIdx
        }

    -- |
    --
    makeResponse :: Gi.GHCi (Either String D.ScopesResponseBody)
    makeResponse = return $ Right D.ScopesResponseBody {
      D.scopesScopesResponseBody = [
        D.defaultScope{
            D.nameScope = _GHCi_SCOPE
          , D.variablesReferenceScope = 1
          , D.namedVariablesScope = Nothing
          , D.indexedVariablesScope = Nothing
          , D.expensiveScope = False
          }
        ,
        D.defaultScope{
            D.nameScope = _GHCi_GLOBAL_SCOPE
          , D.variablesReferenceScope = 2
          , D.namedVariablesScope = Nothing
          , D.indexedVariablesScope = Nothing
          , D.expensiveScope = False
          }
        ]
      }

    -- |
    --
    withMoveIdx :: Int -> Gi.GHCi [GAC.TyThing]
    withMoveIdx moveIdx
      | 0 == moveIdx = G.getBindings
      | 0 < moveIdx = back moveIdx
      | otherwise = forward (negate moveIdx)


#if __GLASGOW_HASKELL__ >= 904
    -- |
    --
    getGlobalBindings :: Gi.GHCi [GAC.TyThing]
    getGlobalBindings = GAC.withSession $ \hsc_env -> do
      let ic = GAC.hsc_IC hsc_env
          gb = GAC.icReaderEnv ic
          es = GAC.globalRdrEnvElts gb
          ns = foldr contName [] $ map GAC.gre_name es
      foldM withName [] $ reverse ns

      where
        contName :: GAC.GreName -> [G.Name] -> [G.Name]
        contName (GAC.NormalGreName n) xs = n:xs
        contName (GAC.FieldGreName _)  xs = xs
#elif __GLASGOW_HASKELL__ >= 902
    -- |
    --
    getGlobalBindings :: Gi.GHCi [GAC.TyThing]
    getGlobalBindings = GAC.withSession $ \hsc_env -> do
      let ic = GAC.hsc_IC hsc_env
          gb = GAC.ic_rn_gbl_env ic
          es = GAC.globalRdrEnvElts gb
          ns = foldr contName [] $ map GAC.gre_name es
      foldM withName [] $ reverse ns

      where
        contName :: GAC.GreName -> [G.Name] -> [G.Name]
        contName (GAC.NormalGreName n) xs = n:xs
        contName (GAC.FieldGreName _)  xs = xs
#else
    -- |
    --
    getGlobalBindings :: Gi.GHCi [GAC.TyThing]
    getGlobalBindings = GAC.withSession $ \hsc_env -> do
      let ic = GAC.hsc_IC hsc_env
          gb = GAC.ic_rn_gbl_env ic
          es = GAC.globalRdrEnvElts gb
          ns = map GAC.gre_name es
      foldM withName [] $ reverse ns
#endif

    -- |
    --
    back :: Int -> Gi.GHCi [GAC.TyThing]
    back num = do
      clearBindingNames
      Gi.backCmd $ show num
      names <- getBindingNames
      foldM withName [] $ reverse names

    -- |
    --
    forward :: Int -> Gi.GHCi [GAC.TyThing]
    forward num = do
      clearBindingNames
      Gi.forwardCmd $ show num
      names <- getBindingNames
      foldM withName [] $ reverse names

    -- |
    --
    withName :: [GAC.TyThing] -> G.Name -> Gi.GHCi [GAC.TyThing]
    withName acc n = G.lookupName n >>= \case
      Just ty -> return (ty : acc)
      Nothing ->  do
        dflags <- GAC.getDynFlags
        errorL $ "variable not found. " ++ GAC.showSDoc dflags (GAC.ppr n)
        return acc


------------------------------------------------------------------------------------------------
--  DAP Command :dap-variables
------------------------------------------------------------------------------------------------
-- |
--
dapVariablesCmd :: String -> Gi.GHCi ()
dapVariablesCmd argsStr = flip gcatch errHdl $ do
  decodeDAP argsStr
  >>= dapVariablesCmd_
  >>= printDAP

-- |
--
dapVariablesCmd_ :: D.VariablesRequestArguments
                 -> Gi.GHCi (Either String D.VariablesResponseBody)
dapVariablesCmd_ args = do
  let idx  = D.variablesReferenceVariablesRequestArguments args
  vals <- getBindingVariables idx
  return $ Right $ D.VariablesResponseBody $ L.sortBy compName vals
  where
    -- |
    --
    compName :: D.Variable -> D.Variable -> Ordering
    compName a b = compare (D.nameVariable a) (D.nameVariable b)


-- |
--
getBindingVariables :: Int -> Gi.GHCi [D.Variable]
getBindingVariables idx
  | 1 == idx  = getBindingVariablesLocal
  | 2 == idx  = getBindingVariablesGlobal
  | otherwise = getBindingVariablesNode idx


-- |
--
getBindingVariablesLocal :: Gi.GHCi [D.Variable]
getBindingVariablesLocal = do
  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
  bindings <- liftIO $ bindingDAPContext <$> readMVar ctxMVar
  getBindingVariablesRoot bindings


-- |
--
getBindingVariablesGlobal :: Gi.GHCi [D.Variable]
getBindingVariablesGlobal = do
  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
  bindings <- liftIO $ bindingGlobalDAPContext <$> readMVar ctxMVar
  getBindingVariablesRoot bindings


-- |
--
getBindingVariablesRoot :: [G.TyThing] -> Gi.GHCi [D.Variable]
getBindingVariablesRoot bindings = do
  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
  ctx <- liftIO $ readMVar ctxMVar
  let isInspect = isInspectVariableDAPContext ctx
  mapM (tyThing2Var isInspect) bindings


-- |
--
getBindingVariablesNode :: Int -> Gi.GHCi [D.Variable]
getBindingVariablesNode idx = do
  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
  ctx <- liftIO $ readMVar ctxMVar
  case M.lookup idx (variableReferenceMapDAPContext ctx) of
    Just (t, str) -> term2Vars t str
    Nothing       -> throwError $ "variable id:" ++ show idx ++ " not found."

  where
    -- |
    --
    term2Vars :: GAC.Term -> String -> Gi.GHCi [D.Variable]
    term2Vars (GAC.Term _ (Right dc) _ subTerms) str = do
      let labels = if 0 == length (GAC.dataConFieldLabels dc)
                     then map (\i->"_" ++ show i) [1..(length subTerms)]
                     else map (GAC.unpackFS . GAC.flLabel) (GAC.dataConFieldLabels dc)
      mapM (flip term2Var str) $ zip labels subTerms

    term2Vars (GAC.Term _ (Left _) _ subTerms) str = do
      let labels = map (\i->"_" ++ show i) [1..(length subTerms)]
      mapM (flip term2Var str) $ zip labels subTerms

    term2Vars t str = do
      dflags <- GAC.getDynFlags
      let tstr = GAC.showSDoc dflags (GAC.ppr t)
      warnL $ "unsupported map term type. " ++ tstr ++ ". idx:" ++ show idx ++ ", name:" ++ str
      return []


------------------------------------------------------------------------------------------------
--  DAP Command :dap-evaluate
------------------------------------------------------------------------------------------------
-- |
--
dapEvalCmd :: String -> Gi.GHCi ()
dapEvalCmd argsStr = flip gcatch errHdl $ do
  decodeDAP argsStr
  >>= dapEvalCmd_
  >>= printDAP

-- |
--
dapEvalCmd_ :: D.EvaluateRequestArguments
            -> Gi.GHCi (Either String D.EvaluateResponseBody)
dapEvalCmd_ args = case D.contextEvaluateRequestArguments args of
  Nothing      -> runRepl  args
  Just "repl"  -> runRepl  args
  Just "watch" -> runOther args
  Just "hover" -> runOther args
  _            -> runOther args
  where
    -- |
    --
    runRepl ::  D.EvaluateRequestArguments -> Gi.GHCi (Either String D.EvaluateResponseBody)
    runRepl args = runStmt $ D.expressionEvaluateRequestArguments args

    -- |
    --
    runStmt :: String -> Gi.GHCi (Either String D.EvaluateResponseBody)
    runStmt "" =
      return $ Right D.defaultEvaluateResponseBody {
               D.resultEvaluateResponseBody = "no input."
             , D.typeEvaluateResponseBody   = "no input."
             , D.variablesReferenceEvaluateResponseBody = 0
             }
    runStmt stmt = do
      var <- runStmtVar stmt
      return $ Right D.defaultEvaluateResponseBody {
               D.resultEvaluateResponseBody = D.valueVariable var
             , D.typeEvaluateResponseBody   = D.typeVariable var
             , D.variablesReferenceEvaluateResponseBody = D.variablesReferenceVariable var
             }

    -- |
    --
    runOther :: D.EvaluateRequestArguments -> Gi.GHCi (Either String D.EvaluateResponseBody)
    runOther args = do
      let nameStr = D.expressionEvaluateRequestArguments args
      names <- G.parseName nameStr
      var   <- names2Var nameStr names

      let varStr = if "_" == D.valueVariable var
                     then "_ :: " ++ D.typeVariable var
                     else D.valueVariable var
      return $ Right D.defaultEvaluateResponseBody {
               D.resultEvaluateResponseBody = varStr
             , D.typeEvaluateResponseBody   = D.typeVariable var
             , D.variablesReferenceEvaluateResponseBody = D.variablesReferenceVariable var
             }

------------------------------------------------------------------------------------------------
--  DAP Command :dap-continue
------------------------------------------------------------------------------------------------
-- |
--
dapContinueCmd :: String -> Gi.GHCi ()
dapContinueCmd argsStr = flip gcatch errHdl $ do
  decodeDAP argsStr
  >>= dapContinueCmd_
  >>= printDAP

-- |
--
dapContinueCmd_ :: D.ContinueRequestArguments
                -> Gi.GHCi (Either String D.StoppedEventBody)
dapContinueCmd_ args = do
  seb <- case D.exprContinueRequestArguments args of
           Just exp -> startTrace exp
           Nothing  -> continue

  return $ Right seb

  where
    -- |
    --
    startTrace :: String -> Gi.GHCi D.StoppedEventBody
    startTrace expr = do
      clearContinueExecResult

      -- ghci maybe throw error cause of force inspecting variable.
      gcatch (Gi.traceCmd expr) unexpectErrHdl

      handleResult

    -- |
    --
    continue :: Gi.GHCi D.StoppedEventBody
    continue = startTrace ""

    -- |
    --
    handleResult :: Gi.GHCi D.StoppedEventBody
    handleResult = hasBreaked >>= \case
      False -> genStoppedEventBody "breakpoint"
      True  -> isBreakthrough >>= \case
        False -> genStoppedEventBody "breakpoint"
        True  -> continue

    -- |
    --
    hasBreaked :: Gi.GHCi Bool
    hasBreaked = getContinueExecResult >>= \case
      Just G.ExecBreak {G.breakInfo = Just _} -> return True
      _ -> return False

    -- |
    --
    isBreakthrough :: Gi.GHCi Bool
    isBreakthrough = G.getResumeContext >>= \case
      []    -> warnL "invalid resume state. resume not found."
            >> return False
      (r:_) -> pure (G.resumeBreakInfo r)
           >>= Gi.toBreakIdAndLocation
           >>= withBreakInfo

    -- |
    --   @return
    --    True  -> thruough
    --    False -> break
    --
    withBreakInfo ::  Maybe (Int, Gi.BreakLocation) -> Gi.GHCi Bool
    withBreakInfo Nothing = do
      warnL "invalid resume break info state."
      return False

    withBreakInfo (Just (no, _)) = findSrcBP no >>= \case
      Just srcBP -> withSrcBP no srcBP
      Nothing    -> findFuncBP no >>= \case
        Just fncBP -> withFuncBP no fncBP
        Nothing    -> do
          warnL $ "invalid break no. " ++ show no
          return False

    -- |
    --
    findSrcBP :: Int -> Gi.GHCi (Maybe SourceBreakpointInfo)
    findSrcBP no = do
      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      srcBPs <- liftIO $ srcBPsDAPContext <$> readMVar ctxMVar
      return $ M.lookup no srcBPs

    -- |
    --
    findFuncBP :: Int -> Gi.GHCi (Maybe (D.FunctionBreakpoint, Int))
    findFuncBP no = do
      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      funcBPs <- liftIO $ funcBPsDAPContext <$> readMVar ctxMVar
      return $ M.lookup no funcBPs

    -- |
    --   @return
    --    True  -> thruough
    --    False -> break
    --
    withSrcBP :: Int -> SourceBreakpointInfo -> Gi.GHCi Bool
    withSrcBP no bpInfo =
      let bpCond = D.conditionSourceBreakpoint (srcBPSourceBreakpointInfo bpInfo)
          bpLog  = D.logMessageSourceBreakpoint (srcBPSourceBreakpointInfo bpInfo)
      in
        srcBreakthroughCounterHandler no bpInfo >>= \case
          Just res -> return res
          Nothing -> breakthroughCondtionHandler no bpCond >>= \case
            Just res -> return res
            Nothing -> logPointHandler no bpLog >>= \case
              Just res -> return res
              Nothing  -> return False


    -- |
    --   @return
    --    True  -> thruough
    --    False -> break
    --
    withFuncBP :: Int -> (D.FunctionBreakpoint, Int) -> Gi.GHCi Bool
    withFuncBP no bpInfo =
      let bpCond = D.conditionFunctionBreakpoint (fst bpInfo)
      in
        funcBreakthroughCounterHandler no bpInfo >>= \case
          Just res -> return res
          Nothing -> breakthroughCondtionHandler no bpCond >>= \case
            Just res -> return res
            Nothing  -> return False

    -- |
    --
    srcBreakthroughCounterHandler :: Int -> SourceBreakpointInfo -> Gi.GHCi (Maybe Bool)
    srcBreakthroughCounterHandler _ SourceBreakpointInfo {
                                      srcBPSourceBreakpointInfo = D.SourceBreakpoint {
                                        D.hitConditionSourceBreakpoint = Nothing
                                      }
                                    } = return Nothing
    srcBreakthroughCounterHandler no bpInfo@SourceBreakpointInfo {
                                              srcBPSourceBreakpointInfo = D.SourceBreakpoint {
                                                  D.hitConditionSourceBreakpoint = Just condStr
                                              }
                                            , hitCntSourceBreakpointInfo = curCnt} = do
      let newCnt = curCnt + 1
          stmt   = if L.isInfixOf "_CNT" condStr
                     then "let _CNT = " ++ show newCnt ++ " in " ++ condStr
                     else "let _CNT = " ++ show newCnt ++ " in _CNT " ++ condStr

      updateSrcBreakCounter no bpInfo{hitCntSourceBreakpointInfo = newCnt}

      var <- runStmtVar stmt
      when ("Bool" /= D.typeVariable var) $ do
        warnL $ "hit condition statement result type is not Bool. BPNO:" ++ show no ++ " " ++ stmt ++ " -> " ++ show var
      debugL $ "hit condition statement result. " ++ stmt ++ " -> " ++ show var
      return $ Just ("False" == D.valueVariable var)


    -- |
    --
    updateSrcBreakCounter :: Int -> SourceBreakpointInfo -> Gi.GHCi ()
    updateSrcBreakCounter no bpInfo = do
      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      ctx <- liftIO $ takeMVar ctxMVar
      let cur = srcBPsDAPContext ctx
          new = M.insert no bpInfo cur
      liftIO $ putMVar ctxMVar ctx{srcBPsDAPContext = new}


    -- |
    --
    funcBreakthroughCounterHandler :: Int -> (D.FunctionBreakpoint, Int) -> Gi.GHCi (Maybe Bool)
    funcBreakthroughCounterHandler _ (D.FunctionBreakpoint{D.hitConditionFunctionBreakpoint = Nothing}, _) = return Nothing
    funcBreakthroughCounterHandler no info@(D.FunctionBreakpoint{D.hitConditionFunctionBreakpoint = Just condStr}, curCnt) = do
      let newCnt = curCnt + 1
          stmt   = if L.isInfixOf "_CNT" condStr
            then "let _CNT = " ++ show newCnt ++ " in " ++ condStr
            else "let _CNT = " ++ show newCnt ++ " in _CNT " ++ condStr

      updateFuncBreakCounter no (fst info, newCnt)

      var <- runStmtVar stmt
      when ("Bool" /= D.typeVariable var) $ do
        warnL $ "hit condition statement result type is not Bool. BPNO:" ++ show no ++ " " ++ stmt ++ " -> " ++ show var
      debugL $ "hit condition statement result. " ++ stmt ++ " -> " ++ show var
      return $ Just ("False" == D.valueVariable var)


    -- |
    --
    updateFuncBreakCounter :: Int -> (D.FunctionBreakpoint, Int) -> Gi.GHCi ()
    updateFuncBreakCounter no bpInfo = do
      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      ctx <- liftIO $ takeMVar ctxMVar
      let cur = funcBPsDAPContext ctx
          new = M.insert no bpInfo cur
      liftIO $ putMVar ctxMVar ctx{funcBPsDAPContext = new}


    -- |
    --   @return
    --     True  -> breakthrough
    --     False -> break
    --
    breakthroughCondtionHandler :: Int -> Maybe String -> Gi.GHCi (Maybe Bool)
    breakthroughCondtionHandler _ Nothing = return Nothing
    breakthroughCondtionHandler no (Just stmt) = do
      var <- runStmtVar stmt
      when ("Bool" /= D.typeVariable var) $ do
        warnL $ "condition statement result type is not Bool. BPNO:" ++ show no ++ " " ++ stmt ++ " -> " ++ show var
      return $ Just ("False" == D.valueVariable var)


    -- |
    --   @return
    --     must be True -> breakthrough
    --
    logPointHandler :: Int -> Maybe String -> Gi.GHCi (Maybe Bool)
    logPointHandler _ Nothing = return Nothing
    logPointHandler _ (Just stmt) = do
      var <- runStmtVar stmt
      let msg = D.valueVariable var ++ "\n"
          body = D.defaultOutputEventBody { D.outputOutputEventBody = msg
                                          , D.categoryOutputEventBody = "console"}

      printOutputEventDAP (Right body)
      return $ Just True


------------------------------------------------------------------------------------------------
--  DAP Command :dap-next
------------------------------------------------------------------------------------------------
-- |
--
nextCmd :: String -> Gi.GHCi ()
nextCmd argsStr = flip gcatch errHdl $ do
  decodeDAP argsStr
  >>= nextCmd_
  >>= printDAP

-- |
--
nextCmd_ :: D.NextRequestArguments
         -> Gi.GHCi (Either String D.StoppedEventBody)
nextCmd_ _ = do
  clearContinueExecResult

  -- ghci maybe throw error cause of force inspecting variable.
  gcatch (Gi.stepLocalCmd "") unexpectErrHdl

  Right <$> genStoppedEventBody "step"


------------------------------------------------------------------------------------------------
--  DAP Command :dap-step-in
------------------------------------------------------------------------------------------------
-- |
--
stepInCmd :: String -> Gi.GHCi ()
stepInCmd argsStr = flip gcatch errHdl $ do
  decodeDAP argsStr
  >>= stepInCmd_
  >>= printDAP

-- |
--
stepInCmd_ :: D.StepInRequestArguments
          -> Gi.GHCi (Either String D.StoppedEventBody)
stepInCmd_ _ = do
  clearContinueExecResult

  -- ghci maybe throw error cause of force inspecting variable.
  gcatch (Gi.stepCmd "") unexpectErrHdl

  Right <$> genStoppedEventBody "step"


------------------------------------------------------------------------------------------------
--  DAP Command :dap-source
------------------------------------------------------------------------------------------------
-- |
--
sourceCmd :: String -> Gi.GHCi ()
sourceCmd argsStr = flip gcatch errHdl $ do
  decodeDAP argsStr
  >>= sourceCmd_
  >>= printDAP

-- |
--
sourceCmd_ :: D.SourceRequestArguments
          -> Gi.GHCi (Either String D.SourceResponseBody)
sourceCmd_ args = do
  modSums <- Gi.getLoadedModules
  let Just srcInfo =  D.sourceSourceRequestArguments args
      srcPath = D.pathSource srcInfo
      modPaths = map takeModPath modSums
      summary = L.find (\sum -> G.ms_hspp_file sum == srcPath) modSums
  case summary of
    Nothing -> throwError $ "<sourceCmd_> loaded module can not find from path. <" ++ srcPath ++ "> " ++  show modPaths
    Just summary -> do
      case G.ms_hspp_buf summary of
        Nothing -> throwError $ "<sourceCmd_> loaded module can not find from path. <" ++ srcPath ++ "> " ++  show modPaths
        Just strBuf -> do
          let content = SB.lexemeToString strBuf (SB.len strBuf)
          return $ Right D.defaultSourceResponseBody {
              D.contentSourceResponseBody = content
            }
