{-# LANGUAGE LambdaCase #-}

module GHCi.DAP.Utility where

import qualified GHC as G
import qualified GHCi.UI.Monad as Gi hiding (runStmt)

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
