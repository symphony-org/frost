module Frost.Plugins.RholangPlugin where

import Frost.Plugin
import Frost.Effects.Sys
import Frost.Effects.Rholang

import Text.Pandoc
import Polysemy
import PolysemyContrib
import Data.Map.Strict

rholangPlugin :: Member Rholang r => Plugin r
rholangPlugin = justContentPlugin "rholang" (fmap render . exec)
  where
    render out = ([Plain [Str out]], [Str out])
