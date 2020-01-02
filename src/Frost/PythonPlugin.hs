module Frost.PythonPlugin where

import           Frost.Effects.Python
import           Frost.Effects.Sys
import           Frost.Plugin

import           Data.Map.Strict
import           Polysemy
import           PolysemyContrib
import           Text.Pandoc

pythonPlugin :: Member Python r => Plugin r
pythonPlugin = justContentPlugin "python" (fmap render . exec)
  where
    render out = ([Plain [Str out]], [Str out])
