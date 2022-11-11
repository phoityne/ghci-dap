{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified GHCMain as G
import qualified GHCi.UI as G
import Control.Concurrent
import GHCi.DAP.Command
import GHCi.DAP.Type

import Paths_ghci_dap (version)
import Data.Version
import System.IO (stderr, hPutStrLn)

-- |
--  Main
--
main :: IO ()
main = do
  hPutStrLn stderr $ "[DAP][INFO] start ghci-dap-" ++ showVersion version ++ "."

  mvarCtx <- newMVar defaultDAPContext

  let ghciSettings    = G.defaultGhciSettings mvarCtx
      defaultCommands = G.availableCommands ghciSettings
      withDapCommands = defaultCommands ++ dapCommands

  G.ghcMain ghciSettings {G.availableCommands = withDapCommands}
  