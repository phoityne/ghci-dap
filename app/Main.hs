{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified GHCMain as G
import qualified GHCi.UI as G
import Control.Concurrent
import GHCi.DAP.Command
import GHCi.DAP.Type

-- |
--  Main
main :: IO ()
main = do
  mvarCtx <- newMVar defaultDAPContext

  let ghciSettings    = G.defaultGhciSettings mvarCtx
      defaultCommands = G.availableCommands ghciSettings
      withDapCommands = defaultCommands ++ dapCommands

  G.ghcMain ghciSettings {G.availableCommands = withDapCommands}
  