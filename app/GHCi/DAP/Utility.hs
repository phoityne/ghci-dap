{-# LANGUAGE LambdaCase #-}

module GHCi.DAP.Utility where

import qualified GHC as G
import qualified Module as G
import qualified GHCi.UI.Monad as Gi hiding (runStmt)
import qualified GHCi.UI as Gi
import Outputable
import Exception
import qualified Data.Char as CH
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.List as L
import qualified Text.Read as R
import Data.Maybe
import Data.Word
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Monad
import FastString
import HscTypes
import InteractiveEvalTypes
import RtClosureInspect
import PprTyThing
import DynFlags
import DataCon
import Debugger
import qualified Data.Map as M

import GHCi.DAP.Constant
import GHCi.DAP.Type
import qualified Haskell.DAP as D

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
readDAP argsStr = case R.readEither argsStr :: Either String [Word8] of
  Left err -> Left $ "read [Word8] failed. " ++ err ++ " : " ++ argsStr
  Right bs -> case R.readEither (toStr bs) of
    Left err -> Left $ "read response body failed. " ++ err ++ " : " ++  (toStr bs)
    Right a  -> Right a 
  where
    toStr = T.unpack . T.decodeUtf8 . BS.pack


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
clearTmpDAPContext :: Gi.GHCi ()
clearTmpDAPContext = do
  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState 

  ctx <- liftIO $ takeMVar ctxMVar
  liftIO $ putMVar ctxMVar ctx{
      traceCmdExecResultDAPContext   = []
    , doContinueExecResultDAPContext = []
    , runStmtDeclExceptionDAPContext = []
    }


-- |
--
isExceptionResume :: G.Resume -> Bool
isExceptionResume (G.Resume{G.resumeBreakInfo = a}) = isNothing a


-- |
--
parseNameErrorHandler :: SomeException -> Gi.GHCi [G.Name]
parseNameErrorHandler e = liftIO $ print e >> return []


-- |
--
showTermErrorHandler :: SomeException -> Gi.GHCi SDoc
showTermErrorHandler e = return $ text $ show e

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
getRunStmtSourceError :: Gi.GHCi String
getRunStmtSourceError = do
  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState 

  ctx <- liftIO $ readMVar ctxMVar
  let errs = runStmtDeclExceptionDAPContext ctx
      msgs = "[DAP][ERROR] error occurred while runStmt." 
            : map show errs

  return $ L.intercalate "\n" msgs


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


-- |
--
decodeDAP :: Read a => String -> Gi.GHCi a
decodeDAP argsStr = liftEither (readDAP argsStr)

-- |
--
liftEither :: Show a => Either a b -> Gi.GHCi b
liftEither (Left  e) = liftIO $ Exception.throwIO $ userError $ show e
liftEither (Right x) = return x

-- |
--
errHdl :: SomeException -> Gi.GHCi ()
errHdl e = pure (displayException e) >>= printDAP

-- |
--
throwError :: String -> Gi.GHCi a
throwError = liftIO . throwIO . userError


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


