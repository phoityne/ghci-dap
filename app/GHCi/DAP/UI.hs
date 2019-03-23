{-# LANGUAGE LambdaCase #-}

module GHCi.DAP.UI where

import qualified GHC as G
import qualified GHCi.UI.Monad as Gi hiding (runStmt)
import Control.Concurrent
import Control.Monad.IO.Class
import InteractiveEval

import GHCi.DAP.Type

-- | 
--
setStackTraceResult :: G.Resume -> [G.History] -> Gi.GHCi ()
setStackTraceResult r hs = do
  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
  ctx <- liftIO $ takeMVar ctxMVar
  liftIO $ putMVar ctxMVar ctx {stackTraceResultDAPContext = Just (r, hs)}


-- | 
--
setBindingNames :: [G.Name] -> Gi.GHCi ()
setBindingNames names = do
  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState
  ctx <- liftIO $ takeMVar ctxMVar
  liftIO $ putMVar ctxMVar ctx {bindingNamesDAPContext = names}

{-
-- |
--
saveTraceCmdExecResult :: Maybe ExecResult -> Gi.GHCi (Maybe ExecResult)
saveTraceCmdExecResult res = do
  ctxMVar <- Gi.dapContextGHCiState  <$> Gi.getGHCiState 

  ctx <- liftIO $ takeMVar ctxMVar
  let cur = traceCmdExecResultDAPContext ctx

  liftIO $ putMVar ctxMVar ctx{traceCmdExecResultDAPContext = res : cur}

  return res


-- |
--
saveDoContinueExecResult :: ExecResult -> Gi.GHCi ExecResult
saveDoContinueExecResult res = do
  ctxMVar <- Gi.dapContextGHCiState <$> Gi.getGHCiState 

  ctx <- liftIO $ takeMVar ctxMVar
  let cur = doContinueExecResultDAPContext ctx

  liftIO $ putMVar ctxMVar ctx{doContinueExecResultDAPContext = res : cur}

  return res
-}

-- |
--
setContinueExecResult :: Maybe ExecResult -> Gi.GHCi (Maybe ExecResult)
setContinueExecResult res = do
  ctxMVar <- Gi.dapContextGHCiState  <$> Gi.getGHCiState 
  ctx <- liftIO $ takeMVar ctxMVar
  liftIO $ putMVar ctxMVar ctx{continueExecResultDAPContext = res}
  return res

