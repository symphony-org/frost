module Frost.Plugins.PlantUmlPlugin where

import           Frost.Effects.PlantUml
import           Frost.Plugin
import           Polysemy
import           Text.Pandoc

plantUmlPlugin :: Member PlantUml r => Plugin r
plantUmlPlugin = justContentPlugin "plantuml" (fmap render . generatePng)
  where
    render t = ([Plain [Image nullAttr [] (t, t)]], [])
