module Frost.DefaultsMandatoryPlugin where

import Frost.Plugin
import Text.Pandoc
import Polysemy
import PolysemyContrib
import Data.Map.Strict

defaultsMandatoryPlugin :: Plugin r
defaultsMandatoryPlugin = justMetaPlugin "meta.defaults" (return . Meta . insertTitle . unMeta)
  where
    insertTitle = insert "title" (MetaString "Documentation")
