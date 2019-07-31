module Frost.GitPlugin where

import Frost.Effects.Git
import Frost.Plugin
import Text.Pandoc
import Polysemy
import PolysemyContrib
import Data.Map.Strict

gitPlugin :: Member Git r => Plugin r
gitPlugin = Plugin "git:devs" (\_ -> return []) atm
  where
    atm :: Member Git r =>  Meta -> Sem r Meta
    atm meta = do
      devs <- devsList
      return $ Meta $ (insertDevs devs) $ unMeta meta
    insertDevs t = insert "devs" (MetaString $ show t)
