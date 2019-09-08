module Frost.PythonPlugin where

import Frost.Plugin
import Frost.Effects.Sys
import Frost.Effects.Python

import Text.Pandoc
import Polysemy
import PolysemyContrib
import Data.Map.Strict

pythonPlugin :: Member Python r => Plugin r
pythonPlugin = justContentPlugin "python" (\script -> render <$> exec script)
  where
    render out = ([Plain [Str out]], [Str out])
