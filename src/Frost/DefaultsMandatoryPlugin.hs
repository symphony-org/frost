module Frost.DefaultsMandatoryPlugin where

import           Data.Map.Strict
import           Frost.Plugin
import           Polysemy
import           PolysemyContrib
import           Text.Pandoc

defaultsMandatoryPlugin :: Plugin r
defaultsMandatoryPlugin = justMetaPlugin "meta.defaults" (return . Meta . insertTitle . unMeta)
  where
    insertTitle = insert "title" (MetaString "Documentation")
