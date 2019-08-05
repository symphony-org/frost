module FrostError where

data FrostError = PluginNotAvailable String | ExitedWithFailure Int | FrostError String
  deriving (Eq, Show)
