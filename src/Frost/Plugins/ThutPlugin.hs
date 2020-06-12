{-# LANGUAGE OverloadedStrings #-}
module Frost.Plugins.ThutPlugin where

import           Data.Text
import           Frost.Effects.Thut
import           Frost.Plugin
import           Polysemy
import           Text.Pandoc

thutPlugin :: Member Thut r => Text -> (Text -> Sem r Text) -> Plugin r
thutPlugin mode command = justContentPlugin mode (\text -> render <$> command text )
  where
    render out = ([Plain [Str out]], [Str out])

thutPlugins :: Member Thut r => [Plugin r]
thutPlugins = [thutPlugin "thut:eval" eval, thutPlugin "thut:passthrough" passthrough]
