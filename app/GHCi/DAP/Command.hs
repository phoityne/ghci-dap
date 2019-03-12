{-# LANGUAGE LambdaCase #-}

module GHCi.DAP.Command where

import qualified GHC as G
import qualified Module as G
import GhcMonad
import HscTypes
import RdrName
import Outputable
import PprTyThing
import Debugger
import Exception
import FastString
import DataCon
import DynFlags
import RtClosureInspect
import InteractiveEvalTypes 
import qualified GHCi.UI as Gi
import qualified GHCi.UI.Monad as Gi hiding (runStmt)

import Control.Monad.Trans.Class
import Control.Concurrent
import Control.Monad

import qualified Data.Map as M
import qualified Data.List as L

import System.Console.Haskeline

import qualified Haskell.DAP as D
import GHCi.DAP.Type
import GHCi.DAP.Constant
import GHCi.DAP.Utility


-- |
--
dapCommands :: [Gi.Command]
dapCommands = map mkCmd [
    ("dap-echo",            dapCmdRunner dapEcho,                          noCompletion)
  , ("dap-set-breakpoints", dapCmdRunner dapSetBreakpointsCommand,         noCompletion)
  , ("dap-set-function-breakpoints"
                          , dapCmdRunner dapSetFunctionBreakpointsCommand, noCompletion)
  , ("dap-set-function-breakpoint"
                          , dapCmdRunner dapSetFuncBreakpointCommand,      noCompletion)
  , ("dap-delete-breakpoint"
                          , dapCmdRunner dapDeleteBreakpointCommand,       noCompletion)
  , ("dap-scopes",          dapCmdRunner dapScopesCommand,                 noCompletion)
  , ("dap-stacktrace",      dapCmdRunner dapStackTraceCommand,             noCompletion)
  , ("dap-variables",       dapCmdRunner dapVariablesCommand,              noCompletion)
  , ("dap-continue",        dapCmdRunner dapContinueCommand,               noCompletion)
  , ("dap-next",            dapCmdRunner dapNextCommand,                   noCompletion)
  , ("dap-step-in",         dapCmdRunner dapStepInCommand,                 noCompletion)
  , ("dap-evaluate",        dapCmdRunner dapEvaluateCommand,               noCompletion)
  ]
  where
    mkCmd (n,a,c) = Gi.Command {
                    Gi.cmdName = n
                  , Gi.cmdAction = a
                  , Gi.cmdHidden = False
                  , Gi.cmdCompletionFunc = c
                  }


-- |
--
dapCmdRunner :: (String -> Gi.GHCi ())
             -> String
             -> InputT Gi.GHCi Bool
dapCmdRunner cmd str = do
  
  lift $ cmd str

  return False


------------------------------------------------------------------------------------------------
--  DAP Command :dap-echo
------------------------------------------------------------------------------------------------
-- |
--
dapEcho :: String -> Gi.GHCi ()
dapEcho str = liftIO $ putStrLn $ "dap-echo \"" ++ str ++ "\""

      
------------------------------------------------------------------------------------------------
--  DAP Command :dap-set-breakpoints
------------------------------------------------------------------------------------------------
-- |
--
dapSetBreakpointsCommand :: String -> Gi.GHCi ()
dapSetBreakpointsCommand argsStr = do
  withArgs (readDAP argsStr) >>= printDAP

  where
    -- |
    --
    withArgs (Left err) = return $ Left $ err ++ " : " ++ argsStr
    withArgs (Right args) = getModule args >>= \case
      Left msg  -> return $ Left msg
      Right mod -> do
        deleteBreakpoints mod
        addBreakpoints args mod

    -- |
    --
    getModule args = do
      let srcInfo = D.sourceSetBreakpointsRequestArguments args
          srcPath = D.pathSource srcInfo

      modSums <- Gi.getLoadedModules
      let modPaths = map takeModPath modSums

      case filter (isPathMatch srcPath) modPaths of
        ((m, p):[]) -> do
          debugL $ "<dapSetBreakpointsCommand> " ++ p ++ " -> " ++ m
          return $ Right m

        _ -> return $ Left $ "loaded module can not find from path. <" ++ srcPath ++ "> " ++  show modPaths

    -- |
    --
    deleteBreakpoints :: ModuleName -> Gi.GHCi ()
    deleteBreakpoints mod = do
      bps <- getDelBPs mod
      debugL $ "<dapSetBreakpointsCommand> delete src bps " ++ show bps
      mapM_ delBreakpoint bps

    -- |
    --
    getDelBPs :: ModuleName -> Gi.GHCi [Int]
    getDelBPs mod = do
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
    takeModPath ms = (G.moduleNameString (G.ms_mod_name ms), G.ms_hspp_file ms)

    -- |
    --
    isPathMatch srcPath (_, p) = (nzPath srcPath) == (nzPath p)

    -- |
    --
    addBreakpoints :: D.SetBreakpointsRequestArguments -> ModuleName -> Gi.GHCi (Either String D.SetBreakpointsResponseBody)
    addBreakpoints args mod = do
      let srcBPs = D.breakpointsSetBreakpointsRequestArguments args
      addBps <- mapM (addBP mod) srcBPs

      updateBpCtx addBps

      return $ Right $ D.SetBreakpointsResponseBody $ map takeBp addBps
    
    -- |
    --
    addBP :: String -> D.SourceBreakpoint -> Gi.GHCi (ModuleName, D.SourceBreakpoint, D.Breakpoint)
    addBP mod srcBP = do
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
dapSetFunctionBreakpointsCommand :: String -> Gi.GHCi ()
dapSetFunctionBreakpointsCommand argsStr =
  withArgs (readDAP argsStr) >>= printDAP

  where
    withArgs (Left err) = return $ Left $ err ++ " : " ++ argsStr
    withArgs (Right args) =  deleteBreakpoints
                          >> addBreakpoints args
      
    -- |
    --
    deleteBreakpoints :: Gi.GHCi ()
    deleteBreakpoints = do
      bps <- getDelBPs
      debugL $ "<dapSetFunctionBreakpointsCommand> delete func bps " ++ show bps
      mapM_ delBreakpoint bps
      
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
    addBreakpoints :: D.SetFunctionBreakpointsRequestArguments -> Gi.GHCi (Either String D.SetFunctionBreakpointsResponseBody)
    addBreakpoints args = do
      let funcBPs = D.breakpointsSetFunctionBreakpointsRequestArguments args

      addBps <- mapM addBP funcBPs

      updateBpCtx addBps

      return $ Right $ D.SetFunctionBreakpointsResponseBody $ map snd addBps
    
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
--  DAP Command :dap-set-function-breakpoint-adhoc
------------------------------------------------------------------------------------------------

-- |
--
dapSetFuncBreakpointCommand :: String -> Gi.GHCi ()
dapSetFuncBreakpointCommand argsStr =
  withArgs (readDAP argsStr) >>= printDAP

  where
    withArgs (Left err) = return $ Left $ err ++ " : " ++ argsStr
    withArgs (Right (startup, funcBP)) = getModuleByFile startup >>= \case
      Left err -> return $ Left err
      Right modName -> do
        let funcName = D.nameFunctionBreakpoint funcBP
            argStr   = modName ++ "." ++ funcName

        bp <- addBreakpoint argStr
        updateBpCtx (funcBP, bp)

        return $ Right bp

    -- |
    --
    updateBpCtx :: (D.FunctionBreakpoint, D.Breakpoint) -> Gi.GHCi ()
    updateBpCtx (funcBP, bp) = case D.idBreakpoint bp of
      Nothing -> return ()
      Just no -> do
        ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
        ctx <- liftIO $ takeMVar ctxMVar
        let funcBpMap =  funcBPsDAPContext ctx

        liftIO $ putMVar ctxMVar $ ctx{funcBPsDAPContext = M.insert no (funcBP, 0) funcBpMap}


    -- |
    --
    getModuleByFile :: String -> Gi.GHCi (Either String String)
    getModuleByFile srcPath = do

      modSums <- Gi.getLoadedModules
      let modPaths = map takeModPath modSums

      case filter (isPathMatch srcPath) modPaths of
        ((m, p):[]) -> do
          debugL $ "<dapSetFuncBreakpointCommand> " ++ p ++ " -> " ++ m
          return $ Right m
        _ -> return $ Left $ "loaded module can not find from path. <" ++ srcPath ++ "> " ++  show modPaths

      where
        takeModPath ms = (G.moduleNameString (G.ms_mod_name ms), G.ms_hspp_file ms)
        isPathMatch srcPath (_, p) = (nzPath srcPath) == (nzPath p)


------------------------------------------------------------------------------------------------
--  DAP Command :dap-delete-breakpoint-adhoc
------------------------------------------------------------------------------------------------

-- |
--
dapDeleteBreakpointCommand :: String -> Gi.GHCi ()
dapDeleteBreakpointCommand argsStr =
  withArgs (readDAP argsStr) >>= printDAP

  where
    -- |
    --
    withArgs (Left err) = return $ Left $ err ++ " : " ++ argsStr
    withArgs (Right bp) = withBpId $ D.idBreakpoint bp

    -- |
    --
    withBpId Nothing    = return $ Left $ "breakpoint number not found. " ++ show argsStr
    withBpId (Just bid) = do
      delBreakpoint bid
      updateBpCtx bid
      return $ Right ()
      
    -- |
    --
    updateBpCtx :: Int -> Gi.GHCi ()
    updateBpCtx bid = do
      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      ctx <- liftIO $ takeMVar ctxMVar
      let funcBpMap =  funcBPsDAPContext ctx

      liftIO $ putMVar ctxMVar $ ctx{funcBPsDAPContext = M.delete bid funcBpMap}

------------------------------------------------------------------------------------------------

-- |
--
delBreakpoint :: Int -> Gi.GHCi Bool
delBreakpoint bpNoStr = do
  curSt <- Gi.getGHCiState
  let curCount = Gi.break_ctr curSt

  Gi.deleteCmd (show bpNoStr)
  
  newSt <- Gi.getGHCiState
  let newCount = Gi.break_ctr newSt

  return (newCount == curCount - 1)


-- |
--
addBreakpoint :: String -> Gi.GHCi D.Breakpoint
addBreakpoint argStr = do
  curSt <- Gi.getGHCiState
  let curCount = Gi.break_ctr curSt

  Gi.breakCmd argStr
  
  newSt <- Gi.getGHCiState
  let newCount = Gi.break_ctr newSt
      isAdded = (newCount == curCount + 1)
      locMay  =  if isAdded then Just (head (Gi.breaks newSt)) else Nothing
  
  withBreakLoc locMay

  where
    withBreakLoc (Just (no, bpLoc))= withSrcSpan no bpLoc (Gi.breakLoc bpLoc)
    withBreakLoc Nothing = return D.defaultBreakpoint {
        D.verifiedBreakpoint = False
      , D.messageBreakpoint  = "set breakpoint seems to be failed."
      }

    withSrcSpan no bpLoc (G.RealSrcSpan dat) = return
      D.defaultBreakpoint {
        D.idBreakpoint        = Just no
      , D.verifiedBreakpoint  = True
      , D.sourceBreakpoint    = D.defaultSource {
          D.nameSource             = (Just . G.moduleNameString . G.moduleName . Gi.breakModule) bpLoc
        , D.pathSource             = (unpackFS . G.srcSpanFile) dat
        , D.sourceReferenceSource  = Nothing
        , D.origineSource          = Nothing
        }
      , D.lineBreakpoint      = G.srcSpanStartLine dat
      , D.columnBreakpoint    = G.srcSpanStartCol dat
      , D.endLineBreakpoint   = G.srcSpanEndLine dat
      , D.endColumnBreakpoint = G.srcSpanEndCol dat
      }

    withSrcSpan _ _ (G.UnhelpfulSpan _) = return D.defaultBreakpoint {
        D.verifiedBreakpoint = False
      , D.messageBreakpoint  = "UnhelpfulSpan breakpoint."
      }



------------------------------------------------------------------------------------------------
--  DAP Command :dap-stacktrace
------------------------------------------------------------------------------------------------

-- |
--
dapStackTraceCommand :: String -> Gi.GHCi ()
dapStackTraceCommand argsStr =
  resetFrameId >> withArgs (readDAP argsStr) >>= printDAP
  
  where
    -- |
    --
    resetFrameId = do
      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      ctx <- liftIO $ takeMVar ctxMVar
      liftIO $ putMVar ctxMVar ctx {frameIdDAPContext = 0}

    -- |
    --
    withArgs :: Either String D.StackTraceRequestArguments -> Gi.GHCi (Either String D.StackTraceResponseBody)
    withArgs (Left err) = return $ Left $ err ++ " : " ++ argsStr
    withArgs (Right _) = Gi.historyCmdDAP "" >>= \case
      Left _ -> return $ Left "no stacktrace found."
      Right (r, hist) -> if isExceptionResume r
        then goExceptionHdl hist
        else goBreakHdl r hist

    -- |
    --
    goExceptionHdl hist = do
      traces <- mapM resumeHist2stackFrame hist
      let traces' = setFrameIdx 0 traces

      return $ Right D.defaultStackTraceResponseBody {
          D.stackFramesStackTraceResponseBody = traces'
        , D.totalFramesStackTraceResponseBody = length traces'
        }
    
    -- |
    --
    goBreakHdl r hist = do
      let start  = resume2stackframe r
      hists <- mapM resumeHist2stackFrame hist      
      let traces = start : hists
          traces' = setFrameIdx 0 traces

      return $ Right D.defaultStackTraceResponseBody {
          D.stackFramesStackTraceResponseBody = traces'
        , D.totalFramesStackTraceResponseBody = length traces'
        }

    -- |
    --
    setFrameIdx _ [] = []
    setFrameIdx idx (x:xs) = x{D.idStackFrame = idx} : setFrameIdx (idx+1) xs

    -- |
    --
    resume2stackframe r = D.defaultStackFrame {
        D.idStackFrame = 0
      , D.nameStackFrame = (getStackFrameTitle r)
      , D.sourceStackFrame = D.defaultSource {
          D.pathSource = getSrcPath (G.resumeSpan r)
        }
      , D.lineStackFrame = getStartLine (G.resumeSpan r)
      , D.columnStackFrame = getStartCol (G.resumeSpan r)
      , D.endLineStackFrame = getEndLine (G.resumeSpan r)
      , D.endColumnStackFrame = getEndCol (G.resumeSpan r)
      }
      
    -- |
    --
    resumeHist2stackFrame hist = do
      span <- G.getHistorySpan hist
      return D.defaultStackFrame {
        D.idStackFrame = 0
      , D.nameStackFrame = L.intercalate ":" (G.historyEnclosingDecls hist)
      , D.sourceStackFrame = D.defaultSource {
          D.pathSource = getSrcPath span
        }
      , D.lineStackFrame = getStartLine span
      , D.columnStackFrame = getStartCol span
      , D.endLineStackFrame = getEndLine span
      , D.endColumnStackFrame = getEndCol span
      }
    
    -- |
    --
    getStackFrameTitle r =  maybe "unknown" (G.moduleNameString  . G.moduleName . G.breakInfo_module) (G.resumeBreakInfo r)
                         ++ "."
                         ++ G.resumeDecl r

    -- |
    --
    getSrcPath (G.RealSrcSpan dat) = (unpackFS . G.srcSpanFile) dat
    getSrcPath (G.UnhelpfulSpan _) = "UnhelpfulSpan"

    -- |
    --
    getStartLine (G.RealSrcSpan dat) = G.srcSpanStartLine dat
    getStartLine (G.UnhelpfulSpan _) = 0

    -- |
    --
    getStartCol (G.RealSrcSpan dat) = G.srcSpanStartCol dat
    getStartCol (G.UnhelpfulSpan _) = 0

    -- |
    --
    getEndLine (G.RealSrcSpan dat) = G.srcSpanEndLine dat
    getEndLine (G.UnhelpfulSpan _) = 0

    -- |
    --
    getEndCol (G.RealSrcSpan dat) = G.srcSpanEndCol dat
    getEndCol (G.UnhelpfulSpan _) = 0


------------------------------------------------------------------------------------------------
--  DAP Command :dap-scopes
------------------------------------------------------------------------------------------------

-- |
--
dapScopesCommand :: String -> Gi.GHCi ()
dapScopesCommand argsStr = do
  res <- withArgs (readDAP argsStr) 
  printDAP res
  
  where
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right args) = do
      let idx  = D.frameIdScopesRequestArguments args
      getScopesResponseBody idx

    -- |
    --
    getScopesResponseBody :: Int -> Gi.GHCi (Either String D.ScopesResponseBody)
    getScopesResponseBody curIdx = do
      -- liftIO $ putStrLn $ "[DAP][getScopesResponseBody] frame id." ++ frameIdStr
      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      oldIdx <- liftIO $ frameIdDAPContext <$> readMVar ctxMVar
      let moveIdx = curIdx - oldIdx

      tyThings <- withMoveIdx moveIdx
      gobalTT  <- getGlobalBindings

      -- liftIO $ putStrLn $ "[DAP][getScopesResponseBody] tyThings count." ++ show (length tyThings)
      ctx <- liftIO $ takeMVar ctxMVar
      liftIO $ putMVar ctxMVar ctx {
          variableReferenceMapDAPContext = M.empty
        , bindingDAPContext = tyThings
        , bindingGlobalDAPContext = gobalTT
        , frameIdDAPContext = curIdx
        }
    
      return $ Right D.ScopesResponseBody {
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
    withMoveIdx moveIdx
      | 0 == moveIdx = G.getBindings
      | 0 < moveIdx = back moveIdx
      | otherwise = forward (negate moveIdx)
  
    -- |
    --
    getGlobalBindings :: GhcMonad m => m [TyThing]
    getGlobalBindings = withSession $ \hsc_env -> do
      let ic = hsc_IC hsc_env
          gb = ic_rn_gbl_env ic
          es = globalRdrEnvElts gb
          ns = map gre_name es
      foldM withName [] $ reverse ns

    -- |
    --
    back num = do
      names <- Gi.backCmdDAP $ show num
      foldM withName [] $ reverse names

      
    -- |
    --
    forward num = do
      names <- Gi.forwardCmdDAP $ show num
      foldM withName [] $ reverse names
           
    -- |
    --
    withName acc n = G.lookupName n >>= \case
      Just ty -> return (ty : acc)
      Nothing ->  do
        dflags <- getDynFlags
        liftIO $ putStrLn $ "[DAP][ERROR][getScopesResponseBody] variable not found. " ++ showSDoc dflags (ppr n)
        return acc


------------------------------------------------------------------------------------------------
--  DAP Command :dap-variables
------------------------------------------------------------------------------------------------

-- |
--
dapVariablesCommand :: String -> Gi.GHCi ()
dapVariablesCommand argsStr = do
  res <- withArgs (readDAP argsStr) 
  printDAP res

  where
    withArgs :: Either String D.VariablesRequestArguments -> Gi.GHCi (Either String D.VariablesResponseBody)
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right args) = do
      let idx  = D.variablesReferenceVariablesRequestArguments args

      vals <- getBindingVariables idx

      return $ Right $ D.VariablesResponseBody $  L.sortBy compName vals

    compName a b = compare (D.nameVariable a) (D.nameVariable b)

-- |
--
getBindingVariables :: Int -> Gi.GHCi [D.Variable]
getBindingVariables idx
  | 1 == idx = getBindingVariablesLocal
  | 2 == idx = getBindingVariablesGlobal
  | otherwise  = getBindingVariablesNode idx


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
  -- bindings <- liftIO $ bindingDAPContext <$> readMVar ctxMVar
  -- liftIO $ putStrLn $ "[DAP][INFO] bindings " ++ show (length bindings)

  foldM go [] bindings
  --mapM tyThing2Val bindings

  where
    go acc ty = gcatch (doSomething acc ty) (onError acc)
    doSomething acc ty = do
      v <- tyThing2Val ty
      return (v:acc)
    onError :: [D.Variable] -> SomeException -> Gi.GHCi [D.Variable]
    onError acc e = do
      liftIO $ putStrLn $ "[DAP][DEBUG] ERROR: " ++ (show e)
      return acc
      
    -- |
    --  TyThings https://hackage.haskell.org/package/ghc-8.2.1/docs/HscTypes.html#t:TyThing
    --
    tyThing2Val :: G.TyThing -> Gi.GHCi D.Variable
    tyThing2Val (AnId i) = do
      dflags <- getDynFlags
      let nameStr = showSDoc dflags (ppr i)
      if "_result" == nameStr
        then do
          -- liftIO $ putStrLn $ "xxxxxxxxxx _result"
          withId i
        else do
          let isForce = True
              depth   = _BINDING_INSPECT_DEPTH
              
          G.obtainTermFromId depth isForce i >>= withTerm i
   
    tyThing2Val t@(ATyCon c) = do
      dflags <- getDynFlags
      return D.defaultVariable {
        D.nameVariable  = showSDoc dflags (ppr t)
      , D.typeVariable  = showSDoc dflags (ppr c)
      , D.valueVariable = "<define>"
      , D.evaluateNameVariable = Nothing
      , D.variablesReferenceVariable = 0
      }
  
    tyThing2Val t@(AConLike c) = do
      dflags <- getDynFlags
      return D.defaultVariable {
        D.nameVariable  = showSDoc dflags (ppr t)
      , D.typeVariable  = showSDoc dflags (ppr c)
      , D.valueVariable = "<define>"
      , D.evaluateNameVariable = Nothing
      , D.variablesReferenceVariable = 0
      }
    
    tyThing2Val x = do
      dflags <- getDynFlags
      return D.defaultVariable {
        D.nameVariable  = showSDoc dflags (ppr x)
      , D.typeVariable  = "not yet supported tything."
      , D.valueVariable = "not yet supported tything."
      , D.evaluateNameVariable = Nothing
      , D.variablesReferenceVariable = 0
      }

    -- |
    --  Term https://hackage.haskell.org/package/ghc-8.2.1/docs/RtClosureInspect.html
    --
    withTerm ::  G.Id -> Term -> Gi.GHCi D.Variable
    withTerm i t@(Term ty _ _ _) = do
      dflags <- getDynFlags
      termSDoc <- gcatch (showTerm t) showTermErrorHandler
      let nameStr = showSDoc dflags (ppr i)
          typeStr = showSDoc dflags (pprTypeForUser ty)
          valStr  = showSDoc dflags termSDoc

      nextIdx <- getNextIdx t nameStr
      
      return D.defaultVariable {
        D.nameVariable  = nameStr
      , D.typeVariable  = typeStr
      , D.valueVariable = valStr
      , D.evaluateNameVariable = Just nameStr
      , D.variablesReferenceVariable = nextIdx
      }

    withTerm i _ = withId i

    withId i = do
      dflags <- getDynFlags
      idSDoc   <- pprTypeAndContents i

      let (nameStr, typeStr, valStr) = getNameTypeValue (showSDoc dflags idSDoc)

      return D.defaultVariable {
        D.nameVariable  = nameStr
      , D.typeVariable  = typeStr
      , D.valueVariable = valStr
      , D.evaluateNameVariable = Nothing
      , D.variablesReferenceVariable = 0
      }


-- |
--
getNextIdx :: Term -> String -> Gi.GHCi Int
getNextIdx t@(Term ty _ _ subTerms) str = getDynFlags >>= withDynFlags
  where
    withDynFlags dflags
      | 0 == length subTerms = return 0
      | 1 == length subTerms && isPrim (head subTerms)  = return 0
      | "[Char]" == showSDoc dflags (pprTypeForUser ty) = return 0
      | "String" == showSDoc dflags (pprTypeForUser ty) = return 0
      | otherwise = addTerm2VariableReferenceMap t str

getNextIdx t str = addTerm2VariableReferenceMap t str


-- |
--
addTerm2VariableReferenceMap :: Term -> String -> Gi.GHCi Int
addTerm2VariableReferenceMap t str = do
  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
  ctx <- liftIO $ takeMVar ctxMVar
  let curMap = variableReferenceMapDAPContext ctx
      nextId = (M.size curMap) + 10

  liftIO $ putMVar ctxMVar $ ctx {variableReferenceMapDAPContext = M.insert nextId (t, str) curMap}

  return nextId


-- |
--
getDataConstructor :: Term -> Gi.GHCi String
getDataConstructor (Term _ (Left dc) _ _) = return dc
getDataConstructor (Term _ (Right dc) _ _) = do
  dflags <- getDynFlags
  let conStr  = if isTupleDataCon dc then "Tuple" else showSDoc dflags $ ppr $ dataConName dc
      conStr' = if ":" == conStr then "List" else conStr
      typeStr = showSDoc dflags (pprTypeForUser (dataConRepType dc))
  return $ conStr' ++ " :: " ++ typeStr
getDataConstructor _ = return "[getDataConstructor] not supported type."


-- |
--
getBindingVariablesNode :: Int -> Gi.GHCi [D.Variable]
getBindingVariablesNode idx = do
  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
  ctx <- liftIO $ readMVar ctxMVar
  case M.lookup idx (variableReferenceMapDAPContext ctx) of
    Just (t, str)  -> withTerm t str
    Nothing -> do
      liftIO $ putStrLn $ "[DAP][ERROR][getBindingVariablesNode] id not found. " ++ show idx
      return []

  where
    withTerm (Term _ (Right dc) _ subTerms) str = do
      let labels = if 0 == length (dataConFieldLabels dc)
                     then map (\i->"_" ++ show i) [1..(length subTerms)]
                     else map (unpackFS . flLabel) (dataConFieldLabels dc)
      mapM (withSubTerm str) $ zip labels subTerms

    withTerm (Term _ (Left _) _ subTerms) str = do
      let labels = map (\i->"_" ++ show i) [1..(length subTerms)]
      mapM (withSubTerm str) $ zip labels subTerms

    withTerm _ _ = do
      liftIO $ putStrLn $ "[DAP][ERROR][getBindingVariablesNode] invalid map term type. " ++ show idx
      return []

    withSubTerm evalStr (label, t@(Term ty _ _ _)) = do
      -- liftIO $ putStrLn $ "[DEBUG]" ++ "   subTerms. [" ++ show (length subTerms) ++ "]"
      termSDoc <- gcatch (showTerm t) showTermErrorHandler
      dflags <- getDynFlags

      let nameStr = label
          typeStr = showSDoc dflags (pprTypeForUser ty)
          valStr  = showSDoc dflags termSDoc

      nextIdx <- getNextIdx t evalStr
      valStr' <- if 0 == nextIdx then return valStr
                   else  getDataConstructor t
      return D.defaultVariable {
        D.nameVariable  = nameStr
      , D.typeVariable  = typeStr
      , D.valueVariable = valStr'
      , D.evaluateNameVariable = Just evalStr
      , D.variablesReferenceVariable = nextIdx
      }
    withSubTerm evalStr (label, (Prim ty val)) = do
      dflags <- getDynFlags
      return D.defaultVariable {
        D.nameVariable  = label
      , D.typeVariable  = showSDoc dflags (pprTypeForUser ty)
      , D.valueVariable = showSDoc dflags (ppr val)
      , D.evaluateNameVariable = Just evalStr
      , D.variablesReferenceVariable = 0
      }
    withSubTerm evalStr (label, (Suspension _ ty _ _)) = do
      dflags <- getDynFlags
      return D.defaultVariable {
        D.nameVariable  = label
      , D.typeVariable  = showSDoc dflags (pprTypeForUser ty)
      , D.valueVariable = "function :: " ++ showSDoc dflags (pprTypeForUser ty)
      , D.evaluateNameVariable = Just evalStr
      , D.variablesReferenceVariable = 0
      }
    withSubTerm evalStr (label, _) = return D.defaultVariable {
        D.nameVariable  = label
      , D.typeVariable  = "not supported subTerm."
      , D.valueVariable = "not supported subTerm."
      , D.evaluateNameVariable = Just evalStr
      , D.variablesReferenceVariable = 0
      }


------------------------------------------------------------------------------------------------
--  DAP Command :dap-continue
------------------------------------------------------------------------------------------------

-- |
--
dapContinueCommand :: String -> Gi.GHCi ()
dapContinueCommand argsStr = 
  withArgs (readDAP argsStr) >>= withStopResult

  where
  
    -- |
    --
    withArgs :: Either String D.ContinueRequestArguments -> Gi.GHCi (Either String D.StoppedEventBody)
    withArgs (Left err) = return $ Left $ err ++ " : " ++ argsStr
    withArgs (Right args) = case  D.exprContinueRequestArguments args of
      Just expr -> runWithStmtTrace expr
      Nothing   -> runNoStmtTrace


    -- |
    --
    runWithStmtTrace expr = do
      clearTmpDAPContext
      Gi.traceCmd expr
      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      ctx <- liftIO $ readMVar ctxMVar
      withStmtTraceResults $ traceCmdExecResultDAPContext ctx


    -- |
    --
    withStmtTraceResults [] = return $ Left $ "invalid trace arg result."
    withStmtTraceResults (res:[]) = withStmtTraceResult res
    withStmtTraceResults (res:_) = do
      warnL $ "two or more trace arg results. use first result. "
      withStmtTraceResult res
    

    -- |
    --
    withStmtTraceResult (Just res) = withExecResult "breakpoint" res
    withStmtTraceResult Nothing = do
      msg <- getRunStmtSourceError
      return $ Left $ msg


    -- |
    --
    runNoStmtTrace = do
      clearTmpDAPContext
      Gi.traceCmd ""
      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      ctx <- liftIO $ readMVar ctxMVar
      withNoStmtTraceResults $ doContinueExecResultDAPContext ctx


    -- |
    --
    withNoStmtTraceResults [] = return $ Left $ "invalid trace no arg result."
    withNoStmtTraceResults (res:[]) = withExecResult "breakpoint" res
    withNoStmtTraceResults (res:_) = do
      warnL $ "two or more trace no arg results. use first result. "
      withExecResult "breakpoint" res


    -- |
    --
    withStopResult :: Either String D.StoppedEventBody -> Gi.GHCi ()
    withStopResult res@(Right D.StoppedEventBody{D.reasonStoppedEventBody = "breakpoint"}) = breakthrough res
    withStopResult res = printDAP res


    -- |
    --
    breakthrough :: Either String D.StoppedEventBody -> Gi.GHCi ()
    breakthrough res = isBreakthrough >>= \case
      False -> printDAP res
      True  -> runNoStmtTrace >>= withStopResult


    -- |
    --
    isBreakthrough :: Gi.GHCi Bool
    isBreakthrough = G.getResumeContext >>= withResumes

    -- |
    --   @return
    --    True  -> thruough
    --    False -> break
    --
    withResumes :: [G.Resume] -> Gi.GHCi Bool
    withResumes [] = do
      warnL "invalid resume state."
      return False

    withResumes (r:_) =   pure (G.resumeBreakInfo r)
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
    findSrcBP no = do
      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      srcBPs <- liftIO $ srcBPsDAPContext <$> readMVar ctxMVar
      return $ M.lookup no srcBPs

    -- |
    --
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

      runStmtDAP False stmt >>= \case
        Left  err -> do
          errorL $ "hit condition statement fail. " ++ stmt ++ " -> " ++ err
          return $ Just False
        Right res -> do
          infoL $ "hit condition statement result. " ++ stmt ++ " -> " ++ show res
          return $ Just ("False" == D.resultEvaluateResponseBody res)

    -- |
    --
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

      runStmtDAP False stmt >>= \case
        Left  err -> do
          errorL $ "hit condition statement fail. " ++ stmt ++ " -> " ++ err
          return $ Just False
        Right res -> do
          when ("Bool" /= D.typeEvaluateResponseBody res) $ do
            warnL $ "hit condition statement result type is not Bool. BPNO:" ++ show no ++ " " ++ stmt ++ " -> " ++ show res
          return $ Just ("False" == D.resultEvaluateResponseBody res)


    -- |
    --
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
      runStmtDAP False stmt >>= \case
        Left  err -> do
          errorL $ "condition statement fail. BPNO:" ++ show no ++ " " ++ stmt ++ " -> " ++ err
          return $ Just False
        Right res -> do
          when ("Bool" /= D.typeEvaluateResponseBody res) $ do
            warnL $ "condition statement result type is not Bool. BPNO:" ++ show no ++ " " ++ stmt ++ " -> " ++ show res
          return $ Just ("False" == D.resultEvaluateResponseBody res)

    -- |
    --   @return
    --     must be True -> breakthrough
    -- 
    logPointHandler :: Int -> Maybe String -> Gi.GHCi (Maybe Bool)
    logPointHandler _ Nothing = return Nothing
    logPointHandler no (Just stmt) = do
      runStmtDAP False stmt >>= \case
        Left err -> do
          let msg = "logpoint evaluate statement error at BPNO:" ++ show no ++ " error:" ++ err
              body = D.defaultOutputEventBody { D.outputOutputEventBody = msg
                                              , D.categoryOutputEventBody = "stderr" }
          printOutputEventDAP (Right body)
          return $ Just False

        Right res -> do
          let msg = D.resultEvaluateResponseBody res ++ "\n"
              body = D.defaultOutputEventBody { D.outputOutputEventBody = msg
                                              , D.categoryOutputEventBody = "console"}

          printOutputEventDAP (Right body)
          return $ Just True


-- |
--
withExecResult :: String-> G.ExecResult -> Gi.GHCi (Either String D.StoppedEventBody)
withExecResult _ (G.ExecComplete { G.execResult = Right _ }) = do
  return $  Right D.defaultStoppedEventBody {
              D.reasonStoppedEventBody = "complete"
            }
  
withExecResult _ (G.ExecComplete { G.execResult = Left (SomeException e)}) = do
  return $  Right D.defaultStoppedEventBody {
              D.reasonStoppedEventBody = "complete"
            , D.descriptionStoppedEventBody = show e
            , D.textStoppedEventBody = show e
            }

withExecResult reason (G.ExecBreak{G.breakInfo = Just (BreakInfo _ _)}) = do
  return $  Right D.defaultStoppedEventBody {
              D.reasonStoppedEventBody = reason
            }

withExecResult _ (G.ExecBreak{G.breakInfo = Nothing}) = do
  let key = "_exception"
  gcatch (G.parseName key) parseNameErrorHandler >>= names2EvalBody False key >>= \case
    Left  msg  -> return $ Left $ "invalid _exception result." ++ msg
    Right body -> return $ Right D.defaultStoppedEventBody {
        D.reasonStoppedEventBody = "exception"
      , D.descriptionStoppedEventBody = D.resultEvaluateResponseBody body
      , D.textStoppedEventBody = D.resultEvaluateResponseBody body
      }


------------------------------------------------------------------------------------------------
--  DAP Command :dap-next
------------------------------------------------------------------------------------------------

-- |
--
dapNextCommand :: String -> Gi.GHCi ()
dapNextCommand argsStr = do
  res <- withArgs (readDAP argsStr)
  printDAP res

  where
    withArgs :: Either String D.NextRequestArguments -> Gi.GHCi (Either String D.StoppedEventBody)
    withArgs (Left err) = return $ Left $ err ++ " : " ++ argsStr
    withArgs (Right _) = do
      clearTmpDAPContext

      Gi.stepLocalCmd ""

      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      ctx <- liftIO $ readMVar ctxMVar
      withResults $ doContinueExecResultDAPContext ctx


    -- |
    --
    withResults [] = return $ Left $ "invalid stepLocalCmd result."
    withResults (res:[]) = withExecResult "step" res
    withResults (res:_) = do
      warnL $ "two or more stepLocalCmd results. use first result. "
      withExecResult "step" res


------------------------------------------------------------------------------------------------
--  DAP Command :dap-step-in
------------------------------------------------------------------------------------------------

-- |
--
dapStepInCommand :: String -> Gi.GHCi ()
dapStepInCommand argsStr = do
  res <- withArgs (readDAP argsStr)
  printDAP res
  
  where
    withArgs :: Either String D.StepInRequestArguments -> Gi.GHCi (Either String D.StoppedEventBody)
    withArgs (Left err) = return $ Left $ err ++ " : " ++ argsStr
    withArgs (Right _) = do
      clearTmpDAPContext

      Gi.stepCmd ""

      ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
      ctx <- liftIO $ readMVar ctxMVar
      withResults $ doContinueExecResultDAPContext ctx


    -- |
    --
    withResults [] = return $ Left $ "invalid stepCmd result."
    withResults (res:[]) = withExecResult "step" res
    withResults (res:_) = do
      warnL $ "two or more stepCmd results. use first result. "
      withExecResult "step" res


------------------------------------------------------------------------------------------------
--  DAP Command :dap-evaluate
------------------------------------------------------------------------------------------------

-- |
--
dapEvaluateCommand :: String -> Gi.GHCi ()
dapEvaluateCommand argsStr = do
  res <- withArgs (readDAP argsStr) 
  printDAP res

  where
    -- |
    --
    withArgs :: Either String D.EvaluateRequestArguments -> Gi.GHCi (Either String D.EvaluateResponseBody)
    withArgs (Left err) = return $ Left $ err ++ " : " ++ argsStr
    withArgs (Right args) = case D.contextEvaluateRequestArguments args of
      Nothing     -> runRepl args
      Just "repl" -> runRepl args
      _           -> runOther args

    -- |
    --
    runRepl ::  D.EvaluateRequestArguments -> Gi.GHCi (Either String D.EvaluateResponseBody)
    runRepl args
      | null (D.expressionEvaluateRequestArguments args) = return $ Right D.defaultEvaluateResponseBody {
          D.resultEvaluateResponseBody = "no input."
        , D.typeEvaluateResponseBody   = "no input."
        , D.variablesReferenceEvaluateResponseBody = 0
        }
      | otherwise = do
        let stmt = D.expressionEvaluateRequestArguments args
            isRefable = True

        runStmtDAP isRefable stmt

    -- |
    --
    runOther ::  D.EvaluateRequestArguments -> Gi.GHCi (Either String D.EvaluateResponseBody)
    runOther args = do 
      let nameStr = D.expressionEvaluateRequestArguments args
      names <- gcatch (G.parseName nameStr) parseNameErrorHandler
      names2EvalBody True nameStr names


-- |
--
runStmtDAP :: Bool -> String -> Gi.GHCi (Either String D.EvaluateResponseBody)
runStmtDAP isRefable stmt = do
  clearTmpDAPContext

  Gi.runStmt stmt G.RunToCompletion >>= \case
    Nothing -> Left <$> getRunStmtSourceError
    Just (G.ExecBreak _ Nothing) -> return $ Left $ "unexpected break occured while evaluating stmt:" ++ stmt
    Just (G.ExecBreak _ (Just (BreakInfo (G.Module _ modName) idx)))   -> do
      let modStr = G.moduleNameString modName
      return $ Left $  "unexpected break occured. breakNo:" ++ show idx
                    ++ " in " ++ modStr ++ " while evaluating stmt:" ++ stmt
    Just (G.ExecComplete (Left msg) _) -> return $ Left $ "runStmt error. " ++ show msg
    Just (G.ExecComplete (Right names) _) -> names2EvalBody isRefable stmt names
    

-- |
--
--
names2EvalBody :: Bool -> String -> [G.Name] -> Gi.GHCi (Either String D.EvaluateResponseBody)
names2EvalBody isRefable key names
  | 0 == length names = return $ Left $ "Not in scope. " ++ key
  | 1 == length names = withName $ head names
  | otherwise = return $ Left $ "Ambiguous name. " ++ key

  where
    withName n = G.lookupName n >>= \case
      Nothing -> return $ Left $ "TyThing not found. " ++ key
      Just ty -> withTyThing ty

    withTyThing (AnId i) = do
      let isForce = True
          depth   = _EVALUATE_INSPECT_DEPTH
      body  <- G.obtainTermFromId depth isForce i >>= withTerm i
      return $ Right body

    withTyThing x = do
      infoL "withTyThing x Not yet supported."
      dflags <- getDynFlags
      return $ Right D.defaultEvaluateResponseBody {
               D.resultEvaluateResponseBody = showSDoc dflags (ppr x)
             , D.typeEvaluateResponseBody   = showSDoc dflags (ppr x)
             , D.variablesReferenceEvaluateResponseBody = 0
             }

    -- |
    --  Term https://hackage.haskell.org/package/ghc-8.2.1/docs/RtClosureInspect.html
    --
    withTerm :: G.Id -> Term -> Gi.GHCi D.EvaluateResponseBody
    withTerm _ t@(Term ty _ _ _) = do
      dflags <- getDynFlags
      termSDoc <- gcatch (showTerm t) showTermErrorHandler
      let typeStr = showSDoc dflags (pprTypeForUser ty)
          valStr  = showSDoc dflags termSDoc

      nextIdx <- if True == isRefable then getNextIdx t key else return 0
      valStr' <- if 0 == nextIdx then return valStr
                   else  getDataConstructor t

      return D.defaultEvaluateResponseBody {
               D.resultEvaluateResponseBody = delDQ typeStr valStr'
             , D.typeEvaluateResponseBody   = typeStr
             , D.variablesReferenceEvaluateResponseBody = nextIdx
             }

    withTerm _ t@(Prim ty _) = do
      dflags <- getDynFlags
      termSDoc <- gcatch (showTerm t) showTermErrorHandler
      let typeStr = showSDoc dflags (pprTypeForUser ty)
          valStr  = showSDoc dflags termSDoc

      return D.defaultEvaluateResponseBody {
                D.resultEvaluateResponseBody = valStr
              , D.typeEvaluateResponseBody   = typeStr
              , D.variablesReferenceEvaluateResponseBody = 0
              }

    withTerm _ t@(Suspension clsr ty _ _) = do
      dflags <- getDynFlags
      termSDoc <- gcatch (showTerm t) showTermErrorHandler
      let typeStr = "closure(" ++ show clsr ++ ")" ++ " :: " ++ showSDoc dflags (pprTypeForUser ty) ++ " # " ++ showSDoc dflags termSDoc

      liftIO $ putStrLn "[DAP][INFO] Suspension Not yet supported."
      return D.defaultEvaluateResponseBody {
                D.resultEvaluateResponseBody = typeStr
              , D.typeEvaluateResponseBody   = typeStr
              , D.variablesReferenceEvaluateResponseBody = 0
              }

    withTerm _ (NewtypeWrap ty _ wt) = do
      dflags <- getDynFlags
      termSDoc <- gcatch (showTerm wt) showTermErrorHandler
      let typeStr = showSDoc dflags (pprTypeForUser ty)
          valStr  = showSDoc dflags termSDoc

      liftIO $ putStrLn "[DAP][INFO] NewtypeWrap Not yet supported."
      return D.defaultEvaluateResponseBody {
                D.resultEvaluateResponseBody = valStr
              , D.typeEvaluateResponseBody   = typeStr
              , D.variablesReferenceEvaluateResponseBody = 0
              }

    withTerm _ (RefWrap ty wt) = do
      dflags <- getDynFlags
      termSDoc <- gcatch (showTerm wt) showTermErrorHandler
      let typeStr = showSDoc dflags (pprTypeForUser ty)
          valStr  = showSDoc dflags termSDoc

      liftIO $ putStrLn "[DAP][INFO] RefWrap Not yet supported."
      return D.defaultEvaluateResponseBody {
                D.resultEvaluateResponseBody = valStr
              , D.typeEvaluateResponseBody   = typeStr
              , D.variablesReferenceEvaluateResponseBody = 0
              }

    delDQ :: String -> String -> String
    delDQ typ val
      | (typ == "[Char]" || typ == "String")
        && length val > 2
        && head val == '"' && last val == '"' = tail $ init val 
      | otherwise = val

