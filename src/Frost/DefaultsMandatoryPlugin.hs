module Frost.DefaultsMandatoryPlugin where

import Frost.Plugin
import Text.Pandoc
import Polysemy
import PolysemyContrib
import Data.Map.Strict

defaultsMandatoryPlugin :: Plugin r
defaultsMandatoryPlugin = Plugin "meta.defaults" (\_ -> return []) atm
  where
    atm  = return . Meta . insertTitle . unMeta
    insertTitle = insert "title" (MetaString $ "Documentation")
