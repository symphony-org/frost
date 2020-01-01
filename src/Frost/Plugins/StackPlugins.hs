module Frost.Plugins.StackPlugins where

import Frost.Plugin
import Text.Pandoc
import Polysemy
import Frost.Effects.Stack

stackPlugin :: (Member Stack r) => String -> Sem r String -> Plugin r
stackPlugin pluginName stackCommand =
  justContentPlugin pluginName (\_ -> do
    output <- stackCommand
    return (renderBlock output, []))
  where
    renderBlock output = [CodeBlock ("", [], []) output]

stackBuildPlugin :: (Member Stack r) => Plugin r
stackBuildPlugin = stackPlugin "stack:build" build

stackTestPlugin :: (Member Stack r) => Plugin r
stackTestPlugin = stackPlugin "stack:test" test

stackPlugins :: (Member Stack r) => [Plugin r]
stackPlugins = [stackBuildPlugin, stackTestPlugin]
