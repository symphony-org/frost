{-# LANGUAGE TemplateHaskell #-}
module Frost where

import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error
import Text.Pandoc

data DynamicError = DynamicError String
  deriving Show

data DynamicContent m a where
  Transform :: Pandoc -> DynamicContent m (Either DynamicError Pandoc)

makeSem ''DynamicContent

generateDocs :: ( Member (Input Pandoc) r
                , Member (Output Pandoc) r
                , Member DynamicContent r
                , Member (Error DynamicError) r
                ) => Sem r ()
generateDocs  = input >>= transform >>= either throw output

runDynamicContent :: Sem (DynamicContent ': r) a -> Sem r a
runDynamicContent = interpret $ \case
  Transform pandoc -> return $ Right pandoc
