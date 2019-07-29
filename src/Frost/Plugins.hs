module Frost.Plugins where

import Frost
import Frost.Plugin
import Frost.TimestampPlugin
import Frost.DefaultsMandatoryPlugin

import Control.Monad
import Polysemy
import PolysemyContrib
import Text.Pandoc
import Text.Pandoc.Extensions
import Data.Map.Strict
import Data.Traversable

transform :: [Plugin r] -> Pandoc -> Sem r (Either DynamicError Pandoc)
transform plugins (Pandoc meta blocks) = do
  let plugin = (head plugins) -- TODO LOL, use all plugins, not one
  newMeta <- addToMeta plugin meta
  newBlocks <- traverse (extractFrostBlocks plugin) blocks
  return $ Right (Pandoc newMeta (join newBlocks))

  where
    extractFrostBlocks plugin = (\case
        CodeBlock ("",[name],[]) content ->
          if name == "frost:" ++ pluginName plugin then substitute plugin content
          else return [CodeBlock ("",["frost"],[]) name]
        otherwise -> return [otherwise])

plugins :: Member SystemEffect r => [Plugin r]
plugins = [ timestampPlugin
          , defaultsMandatoryPlugin
          ]


