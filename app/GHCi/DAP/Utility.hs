{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module GHCi.DAP.Utility where

import qualified GHC as G
import qualified GHCi.UI.Monad as Gi hiding (runStmt)
import qualified GHCi.UI as Gi
import qualified Data.Char as CH
import Data.Maybe
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Monad
import qualified Data.Map as M
import System.Directory

import qualified GHCi.GhcApiCompat as GAC

import qualified Haskell.DAP as D

import qualified GHCi.DAP
import GHCi.DAP.Constant
import GHCi.DAP.Type

-- |
--
gcatch :: Gi.GHCi a -> (GAC.SomeException -> Gi.GHCi a) -> Gi.GHCi a
#if __GLASGOW_HASKELL__ >= 900
gcatch = GAC.catch
#else
gcatch = GAC.gcatch
#endif

-- |
--
_SLASH :: Char
_SLASH = '/'


-- |
--
_BACK_SLASH :: Char
_BACK_SLASH = '\\'


-- |
--
_SPACES :: [Char]
_SPACES = [' ', '\n', '\t']


-- |
--
lstrip, rstrip, strip :: String -> String
lstrip = dropWhile (flip elem _SPACES)
rstrip = reverse . lstrip . reverse
strip  = lstrip . rstrip


-- |
--
toLower :: String -> String
toLower = map CH.toLower


-- |
--
toUpper :: String -> String
toUpper = map CH.toUpper


-- |
--
win2unixSlash :: String -> String
win2unixSlash = map (\c -> if c == _BACK_SLASH then _SLASH else c)


-- |
--
unix2winSlash :: String -> String
unix2winSlash = map (\c -> if c == _SLASH then _BACK_SLASH else c)


-- |
--   normalized path
--
nzPath :: FilePath -> FilePath
nzPath = drive2lower . win2unixSlash


-- |
--
takeModPath :: GAC.ModSummary -> (String, FilePath)
takeModPath ms = (G.moduleNameString (G.ms_mod_name ms), G.ms_hspp_file ms)


-- |
--
isPathMatch :: FilePath -> ModuleName -> Gi.GHCi Bool
isPathMatch srcPath modPath = do
  srcAbs <- liftIO $ canonicalizePath srcPath
  modAbs <- liftIO $ canonicalizePath modPath
  return $ srcAbs == modAbs


-- |
--
findModule :: FilePath -> [(ModuleName, FilePath)] -> Gi.GHCi (Maybe (ModuleName, FilePath))
findModule _ [] = return Nothing
findModule srcPath ((m, p):xs) = isPathMatch srcPath p >>= \case
  True -> return $ Just (m, p)
  False -> findModule srcPath xs


-- |
--  to lowercase Windows drive letter
--
drive2lower :: FilePath -> FilePath
drive2lower (x : ':' : xs) = CH.toLower x : ':' : xs
drive2lower xs = xs


------------------------------------------------------------------------------------------------
--  DAP Utility
------------------------------------------------------------------------------------------------

-- |
--
--   phoityne -> ghci-dap
--   RequestArgument is encoded. decode to [Word8]
--
readDAP :: Read a => String -> Either String a
readDAP = GHCi.DAP.decode


-- |
--
--   ghci-dap -> phoityne
--   Just show ResponseBody. no need to encode to [Word8]
--
showDAP :: Show a => a -> String
showDAP = show


-- |
--
printDAP :: Show a => a -> Gi.GHCi ()
printDAP dat = do
  let outStr = _DAP_HEADER ++ showDAP dat

  liftIO $ putStrLn outStr

-- |
--
printOutputEventDAP ::  (Either String D.OutputEventBody) -> Gi.GHCi ()
printOutputEventDAP dat = do
  let outStr = _DAP_HEADER_OUTPUT_EVENT ++ showDAP dat

  liftIO $ putStrLn outStr


-- |
--
decodeDAP :: Read a => String -> Gi.GHCi a
decodeDAP argsStr = liftEither (readDAP argsStr)

-- |
--
liftEither :: Show a => Either a b -> Gi.GHCi b
liftEither (Left  e) = liftIO $ GAC.throwIO $ userError $ show e
liftEither (Right x) = return x

-- |
--
errHdl :: GAC.SomeException -> Gi.GHCi ()
errHdl e = do
  let msg = GAC.displayException e
      res = Left msg :: Either String ()
  printDAP res

-- |
--
unexpectErrHdl :: GAC.SomeException -> Gi.GHCi ()
unexpectErrHdl e = do
  warnL $ "ghci says,\n" ++ show e


-- |
--
throwError :: String -> Gi.GHCi a
throwError = liftIO . GAC.throwIO . userError


-- |
--
isExceptionResume :: G.Resume -> Bool
#if __GLASGOW_HASKELL__ >= 912 || __GLASGOW_HASKELL__ >= 910 && __GLASGOW_HASKELL_PATCHLEVEL1__ >= 2
isExceptionResume (G.Resume{G.resumeBreakpointId = a}) = isNothing a
#else
isExceptionResume (G.Resume{G.resumeBreakInfo = a}) = isNothing a
#endif

--------------------------------------------------------------------
-- |
--
errorL :: String -> Gi.GHCi ()
errorL msg = logging ErrorLogLevel msg

-- |
--
warnL :: String -> Gi.GHCi ()
warnL msg = logging WarnLogLevel msg

-- |
--
infoL :: String -> Gi.GHCi ()
infoL msg = logging InfoLogLevel msg

-- |
--
debugL :: String -> Gi.GHCi ()
debugL msg = logging DebugLogLevel msg

-- |
--
logging :: LogLevel -> String -> Gi.GHCi ()
logging l msg = do
  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
  lv <- liftIO $ logLevelDAPContext <$> readMVar ctxMVar
  when (lv >= l) $ do
    liftIO $ putStrLn $ show l ++ msg

--------------------------------------------------------------------


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
  newBl <- Gi.dapLookupMaxBreakLocation
  let newCount = Gi.break_ctr newSt
      isAdded = (newCount == curCount + 1)
      locMay  =  if isAdded then newBl else Nothing

  withBreakLoc locMay

  where
    withBreakLoc (Just (no, bpLoc))= withSrcSpan no bpLoc (Gi.breakLoc bpLoc)
    withBreakLoc Nothing = return D.defaultBreakpoint {
        D.verifiedBreakpoint = False
      , D.messageBreakpoint  = "set breakpoint seems to be failed."
      }

#if __GLASGOW_HASKELL__ >= 900
    withSrcSpan no bpLoc (G.RealSrcSpan dat _) = return
#else
    withSrcSpan no bpLoc (G.RealSrcSpan dat) = return
#endif
      D.defaultBreakpoint {
        D.idBreakpoint        = Just no
      , D.verifiedBreakpoint  = True
      , D.sourceBreakpoint    = D.defaultSource {
          D.nameSource             = (Just . G.moduleNameString . G.moduleName . Gi.breakModule) bpLoc
        , D.pathSource             = (GAC.unpackFS . G.srcSpanFile) dat
        , D.sourceReferenceSource  = Nothing
        , D.originSource           = Nothing
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


--------------------------------------------------------------------

-- |
--
genStoppedEventBody :: String -> Gi.GHCi D.StoppedEventBody
genStoppedEventBody reason = getContinueExecResult >>= \case
  Nothing -> throwError "ExecResult not found."
  Just er -> execResult2StoppedEventBody reason er


-- |
--
execResult2StoppedEventBody :: String -> G.ExecResult -> Gi.GHCi D.StoppedEventBody
execResult2StoppedEventBody _ (G.ExecComplete { G.execResult = Right _ }) = do
  return D.defaultStoppedEventBody {
           D.reasonStoppedEventBody = "complete"
         }

execResult2StoppedEventBody _ (G.ExecComplete { G.execResult = Left (GAC.SomeException e)}) = do
  return D.defaultStoppedEventBody {
           D.reasonStoppedEventBody = "complete"
         , D.descriptionStoppedEventBody = show e
         , D.textStoppedEventBody = show e
         }
#if __GLASGOW_HASKELL__ >= 912 || __GLASGOW_HASKELL__ >= 910 && __GLASGOW_HASKELL_PATCHLEVEL1__ >= 2
execResult2StoppedEventBody reason (G.ExecBreak{G.breakPointId = Just _}) = do
#else
execResult2StoppedEventBody reason (G.ExecBreak{G.breakInfo = Just (GAC.BreakInfo _ _)}) = do
#endif
  return D.defaultStoppedEventBody {
           D.reasonStoppedEventBody = reason
         }

#if __GLASGOW_HASKELL__ >= 912 || __GLASGOW_HASKELL__ >= 910 && __GLASGOW_HASKELL_PATCHLEVEL1__ >= 2
execResult2StoppedEventBody _ (G.ExecBreak{G.breakPointId = Nothing}) = do
#else
execResult2StoppedEventBody _ (G.ExecBreak{G.breakInfo = Nothing}) = do
#endif
  -- have to :force _exception first.
  -- then can show it.
  debugL "stopped by exception"
  gcatch (Gi.forceCmd "_exception") unexpectErrHdl

  let stmt = "show _exception"
  var <- runStmtVar stmt

  return D.defaultStoppedEventBody {
           D.reasonStoppedEventBody = "exception"
         , D.descriptionStoppedEventBody = D.valueVariable var
         , D.textStoppedEventBody = D.valueVariable var
         }


-- |
--
getNextIdx :: GAC.Term -> String -> Gi.GHCi Int
getNextIdx t@(GAC.Term ty _ _ subTerms) str = GAC.getDynFlags >>= withDynFlags
  where
    withDynFlags dflags
      | 0 == length subTerms = return 0
      | 1 == length subTerms && isPrimCont (head subTerms)  = return 0
      | "[Char]" == GAC.showSDoc dflags (GAC.pprTypeForUser ty) = return 0
      | "String" == GAC.showSDoc dflags (GAC.pprTypeForUser ty) = return 0
      | otherwise = addTerm2VariableReferenceMap t str

    isPrimCont GAC.Prim{} = True
    isPrimCont _ = False
getNextIdx t str = addTerm2VariableReferenceMap t str


-- |
--
runStmtVar :: String -> Gi.GHCi D.Variable
runStmtVar stmt = do
  clearRunStmtDeclException

  Gi.runStmt stmt G.RunToCompletion >>= \case
    Nothing -> getRunStmtSourceError >>= throwError
    Just (G.ExecBreak _ Nothing) -> throwError $ "unexpected break occured while evaluating stmt:" ++ stmt
#if __GLASGOW_HASKELL__ >= 912 || __GLASGOW_HASKELL__ >= 910 && __GLASGOW_HASKELL_PATCHLEVEL1__ >= 2
    Just (G.ExecBreak _ (Just (GAC.InternalBreakpointId _ _ (GAC.Module _ modName) idx))) -> do
#else
    Just (G.ExecBreak _ (Just (GAC.BreakInfo (GAC.Module _ modName) idx))) -> do
#endif
      let modStr = G.moduleNameString modName
      let msg    = "unexpected break occured. breakNo:" ++ show idx
                   ++ " in " ++ modStr ++ " while evaluating stmt:" ++ stmt
      throwError msg
    Just (G.ExecComplete (Left msg) _) -> throwError $ "runStmt error. " ++ show msg
    Just (G.ExecComplete (Right names) _) -> names2Var stmt names



-- |
--
names2Var :: String -> [G.Name] -> Gi.GHCi D.Variable
names2Var key names
  | 0 == length names = throwError $ "Not in scope. " ++ key
  | 1 == length names = name2Var key (head names)
  | otherwise = throwError $ "Ambiguous name. " ++ key


-- |
--
name2Var :: String -> G.Name -> Gi.GHCi D.Variable
name2Var key n = G.lookupName n >>= \case
  Nothing -> throwError $ "TyThing not found. " ++ key
  Just ty -> tyThing2Var True ty

--------------------------------------------------------------------------

-- |
--  TyThings https://hackage.haskell.org/package/ghc-8.2.1/docs/HscTypes.html#t:TyThing
--
tyThing2Var :: Bool -> G.TyThing -> Gi.GHCi D.Variable
tyThing2Var _ t@(GAC.AConLike c) = defTy2Var t c
tyThing2Var _ t@(GAC.ATyCon c)   = defTy2Var t c
tyThing2Var _ t@(GAC.ACoAxiom c) = defTy2Var t c
tyThing2Var isInspect (GAC.AnId i) = inspectGID isInspect i


-- |
--
defTy2Var :: (GAC.Outputable a, GAC.Outputable b)
          => a -> b -> Gi.GHCi D.Variable
defTy2Var n t = do
  dflags <- GAC.getDynFlags
  let name = GAC.showSDoc dflags (GAC.ppr n)
      typ  = GAC.showSDoc dflags (GAC.ppr t)
  return D.defaultVariable {
    D.nameVariable  = name
  , D.typeVariable  = typ
  , D.valueVariable = "<define>"
  , D.evaluateNameVariable = Nothing
  , D.variablesReferenceVariable = 0
  }

-- |
--
inspectGID :: Bool -> G.Id -> Gi.GHCi D.Variable
inspectGID False i = gid2Var i
inspectGID True  i = do
  dflags <- GAC.getDynFlags
  case GAC.showSDoc dflags (GAC.ppr i) of
    "_result" -> gid2Var i
    _ -> G.obtainTermFromId _BINDING_INSPECT_DEPTH True i >>= term2VarById i

-- |
--
gid2Var :: G.Id -> Gi.GHCi D.Variable
gid2Var i = do
  dflags <- GAC.getDynFlags
  idSDoc <- GAC.pprTypeAndContents i

  let (nameStr, typeStr, valStr) = getNameTypeValue (GAC.showSDoc dflags idSDoc)

  return D.defaultVariable {
    D.nameVariable  = nameStr
  , D.typeVariable  = typeStr
  , D.valueVariable = valStr
  , D.evaluateNameVariable = Just nameStr
  , D.variablesReferenceVariable = 0
  }

-- |
--
getNameTypeValue :: String -> (String, String, String)
getNameTypeValue str = (strip nameStr, strip typeStr, strip valueStr)
  where
    nameStr  = head $ words str
    typeStr  = unwords $ takeWhile ((/=)"=") $ tail $ tail $ words str
    valueStr = unwords $ tail $ dropWhile ((/=)"=") $ words str


-- |
--
term2VarById :: G.Id -> GAC.Term -> Gi.GHCi D.Variable
term2VarById i t@(GAC.Term _ _ _ _) = do
  dflags <- GAC.getDynFlags
  let nameStr = GAC.showSDoc dflags (GAC.ppr i)
      evalStr = ""
  term2Var (nameStr, t) evalStr
term2VarById i _ = gid2Var i


-- |
--  Term https://hackage.haskell.org/package/ghc-8.2.1/docs/RtClosureInspect.html
--
term2Var :: (String, GAC.Term) -> String -> Gi.GHCi D.Variable
term2Var (label, t@(GAC.Term ty _ _ _)) evalStr = do
  dflags <- GAC.getDynFlags
  termSDoc <- GAC.showTerm t
  let nameStr = label
      typeStr = GAC.showSDoc dflags (GAC.pprTypeForUser ty)
      valStr  = GAC.showSDoc dflags termSDoc
      evlStr  = if null evalStr then nameStr else evalStr

  nextIdx <- getNextIdx t nameStr
  typVal <- if 0 == nextIdx then return valStr
              else getDataConstructor t valStr

  return D.defaultVariable {
    D.nameVariable  = nameStr
  , D.typeVariable  = typeStr
  , D.valueVariable = typVal
  , D.evaluateNameVariable = Just evlStr
  , D.variablesReferenceVariable = nextIdx
  }

term2Var (label, t) _ = do
  dflags <- GAC.getDynFlags
  termSDoc <- GAC.showTerm t
  let nameStr = label
      typeStr = GAC.showSDoc dflags termSDoc
      valStr  = GAC.showSDoc dflags termSDoc

  return D.defaultVariable {
    D.nameVariable  = nameStr
  , D.typeVariable  = typeStr
  , D.valueVariable = valStr
  , D.evaluateNameVariable = Nothing
  , D.variablesReferenceVariable = 0
  }


-- |
--
getDataConstructor :: GAC.Term -> String -> Gi.GHCi String
getDataConstructor (GAC.Term _ (Left dc) _ _)  _ = return dc
getDataConstructor (GAC.Term _ (Right dc) _ _) _ = do
  dflags <- GAC.getDynFlags
  let conStr  = if GAC.isTupleDataCon dc then "Tuple" else GAC.showSDoc dflags $ GAC.ppr $ GAC.dataConName dc
      conStr' = if ":" == conStr then "List" else conStr
      typeStr = GAC.showSDoc dflags (GAC.pprTypeForUser (GAC.dataConRepType dc))
  return $ conStr' ++ " :: " ++ typeStr
getDataConstructor t defVal = do
  dflags <- GAC.getDynFlags
  termSDoc <- GAC.showTerm t
  let tstr = GAC.showSDoc dflags termSDoc
  warnL $ "can not get constructer type. " ++ tstr
  return defVal


--------------------------------------------------------------------------
-- | Accessor to DAPContext
--------------------------------------------------------------------------

-- |
--
clearBindingNames :: Gi.GHCi ()
clearBindingNames = do
  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
  ctx <- liftIO $ takeMVar ctxMVar
  liftIO $ putMVar ctxMVar ctx {bindingNamesDAPContext = []}

-- |
--
getBindingNames :: Gi.GHCi [G.Name]
getBindingNames = do
  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
  ctx <- liftIO $ readMVar ctxMVar
  return $ bindingNamesDAPContext ctx


-- |
--
clearContinueExecResult :: Gi.GHCi ()
clearContinueExecResult = do
  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
  ctx <- liftIO $ takeMVar ctxMVar
  liftIO $ putMVar ctxMVar ctx {continueExecResultDAPContext = Nothing}

-- |
--
getContinueExecResult :: Gi.GHCi (Maybe G.ExecResult)
getContinueExecResult = do
  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
  ctx <- liftIO $ readMVar ctxMVar
  return $ continueExecResultDAPContext ctx


-- |
--
clearRunStmtDeclException :: Gi.GHCi ()
clearRunStmtDeclException = do
  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
  ctx <- liftIO $ takeMVar ctxMVar
  liftIO $ putMVar ctxMVar ctx {runStmtDeclExceptionDAPContext = Nothing}

-- |
--
getRunStmtDeclException :: Gi.GHCi (Maybe GAC.SourceError)
getRunStmtDeclException = do
  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
  ctx <- liftIO $ readMVar ctxMVar
  return $ runStmtDeclExceptionDAPContext ctx


-- |
--
getRunStmtSourceError :: Gi.GHCi String
getRunStmtSourceError = do
  err <- getRunStmtDeclException
  let msg = "[DAP][ERROR] error occurred while runStmt. " ++ show err

  return msg


-- |
--
addTerm2VariableReferenceMap :: GAC.Term -> String -> Gi.GHCi Int
addTerm2VariableReferenceMap t str = do
  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
  ctx <- liftIO $ takeMVar ctxMVar
  let curMap = variableReferenceMapDAPContext ctx
      nextId = (M.size curMap) + 10

  liftIO $ putMVar ctxMVar $ ctx {variableReferenceMapDAPContext = M.insert nextId (t, str) curMap}

  return nextId

