module Frost.TimestampPlugin where

import Frost.Plugin
import Data.Functor
import Text.Pandoc
import Polysemy
import PolysemyContrib
import Data.Map.Strict

timestampMetaPlugin :: Member SystemEffect r => Plugin r
timestampMetaPlugin = justMetaPlugin "timestamp:meta" (\meta -> do
  time <- currentTime
  return $ Meta $ (insertTimestamp time) $ unMeta meta)
  where
    insertTimestamp t= insert "creation" (MetaString $ show t)

timestampPlugin :: Member SystemEffect r => Plugin r
timestampPlugin = justContentPlugin "timestamp" (\_ -> currentTime <&> render)
  where
    render t = [Plain [Str $show t]]
