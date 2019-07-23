{-# LANGUAGE TemplateHaskell #-}
module Frost where

import Polysemy
import Polysemy.Input
import Polysemy.Output
import Text.Pandoc

data DynamicContent m a where
  Transform :: Pandoc -> DynamicContent m Pandoc

makeSem ''DynamicContent

generateDocs :: ( Member (Input Pandoc) r
                , Member (Output Pandoc) r
                , Member DynamicContent r
                ) => Sem r ()
generateDocs  = input >>= transform >>= output
