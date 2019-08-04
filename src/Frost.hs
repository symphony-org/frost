{-# LANGUAGE TemplateHaskell #-}
module Frost where

import Frost.Plugin
import Frost.GitPlugin
import Frost.TimestampPlugin
import Frost.DefaultsMandatoryPlugin
import Frost.Effects.Git
import Frost.Effects.Sys

import Data.List (find)
import Control.Monad
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error
import Text.Pandoc hiding (trace)
import Text.Pandoc.Extensions
import Data.Map.Strict
import Data.Traversable

data DynamicError = PluginNotAvailable String | DynamicError String
  deriving (Eq, Show)

generateDocs :: ( Member (Input Pandoc) r
                , Member (Output Pandoc) r
                , Member (Error DynamicError) r
                ) => (Pandoc -> Sem r Pandoc) -> Sem r ()
generateDocs  transform = input >>= transform >>= output


transform :: Member (Error DynamicError) r => [Plugin r] -> Pandoc -> Sem r Pandoc
transform plugins (Pandoc meta blocks) = do
  let plugin = (head plugins) -- TODO LOL, use all plugins, not one
  newMeta <- addToMeta plugin meta
  newBlocks <- traverse (extractFrostBlocks plugins) blocks
  return $ Pandoc newMeta (join newBlocks)

  where
    extractFrostBlocks :: Member (Error DynamicError) r => [Plugin r] -> Block -> Sem r [Block]
    extractFrostBlocks plugins = (\case
        Para [Code ("",[],[]) name] -> do
          let maybePlugin = find (\p -> "frost:" ++ pluginName p == name) plugins
          case maybePlugin of
            Just plugin ->  substitute plugin ""
            Nothing -> throw $ PluginNotAvailable name
        CodeBlock ("",[name],[]) content -> do
          let maybePlugin = find (\p -> "frost:" ++ pluginName p == name) plugins
          case maybePlugin of
            Just plugin ->  substitute plugin content
            Nothing -> throw $ PluginNotAvailable name
        otherwise -> return [otherwise])

plugins :: (Member Sys r, Member Git r) => [Plugin r]
plugins = [ timestampPlugin
          , timestampMetaPlugin
          , defaultsMandatoryPlugin
          , gitPlugin
          ]
