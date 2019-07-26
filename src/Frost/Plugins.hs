module Frost.Plugins where

import Frost
import Frost.Plugin
import Frost.TimestampPlugin
import Frost.DefaultsMandatoryPlugin

import Polysemy
import PolysemyContrib
import Text.Pandoc
import Text.Pandoc.Extensions
import Data.Map.Strict
import Data.Traversable

transform :: [Plugin r] -> Pandoc -> Sem r (Either DynamicError Pandoc)
transform plugins (Pandoc meta blocks) = do
  newMeta <- addToMeta (head plugins) meta
  return $ Right (Pandoc newMeta blocks)

plugins :: Member SystemEffect r => [Plugin r]
plugins = [ timestampPlugin
          , defaultsMandatoryPlugin
          ]


