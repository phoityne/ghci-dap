{-# LANGUAGE LambdaCase #-}

module GHCi.DAP.Command where

import qualified GHC as G
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
    ("dap-set-breakpoints",          dapCmdRunner setBpCmd,         noCompletion)
  , ("dap-set-function-breakpoints", dapCmdRunner setFuncBpsCmd,    noCompletion)
  , ("dap-set-function-breakpoint",  dapCmdRunner setFuncBpCmd,     noCompletion)
  , ("dap-delete-breakpoint",        dapCmdRunner delBpCmd,         noCompletion)
  , ("dap-stacktrace",               dapCmdRunner dapStackTraceCmd, noCompletion)
  , ("dap-scopes",          dapCmdRunner dapScopesCommand,                 noCompletion)
  , ("dap-variables",       dapCmdRunner dapVariablesCommand,              noCompletion)
  , ("dap-evaluate",        dapCmdRunner dapEvaluateCommand,               noCompletion)
  , ("dap-continue",        dapCmdRunner dapContinueCommand,               noCompletion)
  , ("dap-next",                     dapCmdRunner nextCmd,          noCompletion)
  , ("dap-step-in",                  dapCmdRunner stepInCmd,        noCompletion)
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

      case filter (isPathMatch srcPath) modPaths of
        ((m, p):[]) -> do
          debugL $ "<dapSetBreakpointsCommand> " ++ p ++ " -> " ++ m
          return m

        _ -> throwError $ "loaded module can not find from path. <" ++ srcPath ++ "> " ++  show modPaths

    -- |
    --
    takeModPath :: ModSummary -> (String, FilePath)
    takeModPath ms = (G.moduleNameString (G.ms_mod_name ms), G.ms_hspp_file ms)

    -- |
    --
    isPathMatch :: FilePath -> (String, FilePath) -> Bool
    isPathMatch srcPath (_, p) = (nzPath srcPath) == (nzPath p)

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

      case filter (isPathMatch startup) modPaths of
        ((m, p):[]) -> do
          debugL $ "<dapSetFuncBreakpointCommand> " ++ p ++ " -> " ++ m
          return m
        _ -> throwError $ "loaded module can not find from path. <" ++ startup ++ "> " ++  show modPaths

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

      return $ Right D.defaultStackTraceResponseBody {
          D.stackFramesStackTraceResponseBody = traceWithId
        , D.totalFramesStackTraceResponseBody = length traceWithId
        }

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
    genStackFrame (G.RealSrcSpan dat) name = D.defaultStackFrame {
        D.idStackFrame        = 0
      , D.nameStackFrame      = name
      , D.sourceStackFrame    = D.defaultSource {D.pathSource = (unpackFS . G.srcSpanFile) dat}
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
      clearBindingNames
      Gi.backCmd $ show num
      names <- getBindingNames
      foldM withName [] $ reverse names

      
    -- |
    --
    forward num = do
      clearBindingNames
      Gi.forwardCmd $ show num
      names <- getBindingNames
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
  clearTmpDAPContext

  Gi.stepLocalCmd ""

  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
  ctx <- liftIO $ readMVar ctxMVar
  withResults $ doContinueExecResultDAPContext ctx
  where
    -- |
    --
    withResults [] = throwError "invalid stepLocalCmd result."
    withResults (res:[]) = withExecResult "step" res
    withResults (res:_) = do
      warnL $ "two or more stepLocalCmd results. use first result. "
      withExecResult "step" res


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
  clearTmpDAPContext

  Gi.stepCmd ""

  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
  ctx <- liftIO $ readMVar ctxMVar
  withResults $ doContinueExecResultDAPContext ctx

  where
    -- |
    --
    withResults [] = throwError "invalid stepCmd result."
    withResults (res:[]) = withExecResult "step" res
    withResults (res:_) = do
      warnL $ "two or more stepLocalCmd results. use first result. "
      withExecResult "step" res


