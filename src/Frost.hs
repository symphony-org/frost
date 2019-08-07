{-# LANGUAGE TemplateHaskell #-}
module Frost where

import FrostError
import Frost.Plugin
import Frost.GitContributorsPlugin
import Frost.TimestampPlugin
import Frost.DefaultsMandatoryPlugin
import Frost.Effects.Git
import Frost.Effects.Sys

import Data.Foldable
import Data.Functor ((<&>))
import Data.List (find)
import Data.List.Utils (split)
import Control.Monad
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error
import Text.Pandoc hiding (trace)
import Text.Pandoc.Extensions
import Data.Map.Strict hiding (split)
import Data.Traversable

generateDocs :: ( Member (Input Pandoc) r
                , Member (Output Pandoc) r
                , Member (Error FrostError) r
                ) => (Pandoc -> Sem r Pandoc) -> Sem r ()
generateDocs  transform = input >>= transform >>= output


transform :: Member (Error FrostError) r => [Plugin r] -> Pandoc -> Sem r Pandoc
transform plugins (Pandoc meta blocks) = do
  newMeta <- (foldM (\m -> \p -> (addToMeta p m)) meta plugins)
  newBlocks <- traverse (replaceBlock plugins) blocks
  return $ Pandoc newMeta (join newBlocks)

  where
    replaceBlock :: Member (Error FrostError) r => [Plugin r] -> Block -> Sem r [Block]
    replaceBlock plugins = (\case
        Para inlines -> traverse (replaceInline plugins) inlines >>= (pure . pure . Para . join)
        CodeBlock ("",[name],[]) content -> do
          let maybePlugin = find (\p -> "frost:" ++ pluginName p == name) plugins
          case maybePlugin of
            Just plugin ->  substitute plugin content <&> fst
            Nothing -> throw $ PluginNotAvailable name
        otherwise -> return [otherwise])

    replaceInline :: Member (Error FrostError) r => [Plugin r] -> Inline -> Sem r [Inline]
    replaceInline plugins (Code ("",[],[]) nameAndContent) = do
      let (name:contents) = split " " nameAndContent
      let content = join contents
      let maybePlugin = find (\p -> "frost:" ++ pluginName p == name) plugins
      case maybePlugin of
        Just plugin ->  substitute plugin content <&> snd
        Nothing -> throw $ PluginNotAvailable name
    replaceInline plugins otherwise = return [otherwise]
      
plugins :: (Member Sys r, Member Git r) => [Plugin r]
plugins = [ timestampPlugin
          , timestampMetaPlugin
          , defaultsMandatoryPlugin
          , gitContributorsPlugin
          ]
