module Frost.TimestampPlugin where

import Frost.Plugin
import Data.Functor
import Text.Pandoc
import Polysemy
import PolysemyContrib
import Data.Map.Strict

timestampMetaPlugin :: Member SystemEffect r => Plugin r
timestampMetaPlugin = Plugin "timestamp:meta" (\_ -> return []) atm
  where
    atm :: Member SystemEffect r =>  Meta -> Sem r Meta
    atm meta = do
      time <- currentTime
      return $ Meta $ (insertTimestamp time) $ unMeta meta
    insertTimestamp t= insert "creation" (MetaString $ show t)

timestampPlugin :: Member SystemEffect r => Plugin r
timestampPlugin = Plugin "timestamp" sub pure
  where
    sub = (\_ ->  currentTime <&> render)
    render t = [Plain [Str $show t]]
