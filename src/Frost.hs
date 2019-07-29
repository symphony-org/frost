{-# LANGUAGE TemplateHaskell #-}
module Frost where

import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error
import Text.Pandoc

data DynamicError = PluginNotAvailable String | DynamicError String
  deriving (Eq, Show)

generateDocs :: ( Member (Input Pandoc) r
                , Member (Output Pandoc) r
                , Member (Error DynamicError) r
                ) => (Pandoc -> Sem r Pandoc) -> Sem r ()
generateDocs  transform = input >>= transform >>= output


