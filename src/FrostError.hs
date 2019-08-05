module FrostError where

data DynamicError = PluginNotAvailable String | ExitedWithFailure Int | DynamicError String
  deriving (Eq, Show)
