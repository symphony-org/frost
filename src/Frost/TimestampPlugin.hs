module Frost.TimestampPlugin where

import Frost.Plugin
import Frost.Effects.Sys

import Data.Functor
import Text.Pandoc
import Polysemy
import Data.Map.Strict

timestampMetaPlugin :: Member Sys r => Plugin r
timestampMetaPlugin = justMetaPlugin "timestamp:meta" (\meta -> do
  time <- currentTime
  return $ Meta $ (insertTimestamp time) $ unMeta meta)
  where
    insertTimestamp t= insert "creation" (MetaString $ show t)

timestampPlugin :: Member Sys r => Plugin r
timestampPlugin = justContentPlugin "timestamp" (\_ -> currentTime <&> render)
  where
    render t = ([Plain [Str $ show t]], [Str $ show t])
