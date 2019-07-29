module Frost.TimestampPlugin where

import Frost.Plugin
import Text.Pandoc
import Polysemy
import PolysemyContrib
import Data.Map.Strict

timestampPlugin :: Member SystemEffect r => Plugin r
timestampPlugin = Plugin "timestamp:meta" (\_ -> return []) atm
  where
    atm :: Member SystemEffect r =>  Meta -> Sem r Meta
    atm meta = do
      time <- currentTime
      return $ Meta $ (insertTimestamp time) $ unMeta meta
    insertTimestamp t= insert "creation" (MetaString $ show t)
