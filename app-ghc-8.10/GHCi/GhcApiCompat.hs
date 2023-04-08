module GHCi.GhcApiCompat (
    module GhcMonad
  , module RdrName
  , module Outputable
  , module Exception
  , module FastString
  , module DataCon
  , module DynFlags
  , module RtClosureInspect
  , module HscTypes
  , module Debugger
  , module InteractiveEvalTypes
  , module PprTyThing
  , module Module
  , parseName
  , fls2fs
  ) where

import GhcMonad
import RdrName
import Outputable
import Exception
import FastString
import DataCon
import DynFlags
import RtClosureInspect
import HscTypes
import Debugger
import InteractiveEvalTypes
import PprTyThing
import Module
import qualified GHC as G

-- |
--
--
parseName :: GhcMonad m => String -> m [G.Name]
parseName = G.parseName

-- |
--
--
fls2fs :: FieldLabelString -> FastString
fls2fs = id

