module Frost.Plugins.RholangPlugin where

import           Frost.Effects.Rholang
import           Frost.Effects.Sys
import           Frost.Plugin

import           Data.Map.Strict
import           Polysemy
import           PolysemyContrib
import           Text.Pandoc

rholangPlugin :: Member Rholang r => Plugin r
rholangPlugin = justContentPlugin "rholang" (fmap render . exec)
  where
    render out = ([Plain [Str out]], [Str out])
