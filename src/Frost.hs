
module Frost where

import FrostError
import Frost.Plugin
import Frost.PythonPlugin
import Frost.GitContributorsPlugin
import Frost.TimestampPlugin
import Frost.DefaultsMandatoryPlugin
import Frost.Plugins.StackPlugins
import Frost.Plugins.RholangPlugin
import Frost.Effects.Git
import Frost.Effects.Python
import Frost.Effects.Rholang
import Frost.Effects.Sys
import Frost.Effects.Stack

import Data.Foldable
import Data.Functor ((<&>))
import Data.List (find)
import Data.List.Utils (split)
import Control.Monad
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error
import Text.Pandoc
import Text.Pandoc.Extensions
import Data.Map.Strict hiding (split)
import Data.Traversable

{-# ANN module "HLint: ignore Used otherwise as a pattern" #-}

generateDocs :: ( Member (Input Pandoc) r
                , Member (Output Pandoc) r
                , Member (Error FrostError) r
                ) => (Pandoc -> Sem r Pandoc) -> Sem r ()
generateDocs  transform = input >>= transform >>= output


transform :: Member (Error FrostError) r => [Plugin r] -> Pandoc -> Sem r Pandoc
transform plugins (Pandoc meta blocks) = do
  newMeta <- foldM (flip addToMeta) meta plugins
  newBlocks <- traverse (replaceBlock plugins) blocks
  return $ Pandoc newMeta (join newBlocks)

  where
    replaceBlock :: Member (Error FrostError) r => [Plugin r] -> Block -> Sem r [Block]
    replaceBlock plugins = \case
        Para inlines -> pure . Para . join <$> traverse (replaceInline plugins) inlines
        CodeBlock ("",[name],[]) content -> replace plugins name content <&> fst
        otherwise -> return [otherwise]

    replaceInline :: Member (Error FrostError) r => [Plugin r] -> Inline -> Sem r [Inline]
    replaceInline plugins (Code ("",[],[]) nameAndContent) = do
      let (name:contents) = split " " nameAndContent
      let content = join contents
      replace plugins name content <&> snd
    replaceInline plugins otherwise = return [otherwise]

    replace :: Member (Error FrostError) r => [Plugin r] -> String -> String -> Sem r ([Block], [Inline])
    replace plugins name content = do
      let maybePlugin = find ((name ==) . ("frost:" ++) . pluginName) plugins
      case maybePlugin of
        Just plugin ->  substitute plugin content
        Nothing -> throw $ PluginNotAvailable name

plugins :: Members [Git, Python, Rholang, Sys, Stack] r  => [Plugin r]
plugins = [ timestampPlugin
          , timestampMetaPlugin
          , defaultsMandatoryPlugin
          , gitContributorsPlugin
          , pythonPlugin
          , rholangPlugin
          ] ++ stackPlugins
