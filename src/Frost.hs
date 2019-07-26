{-# LANGUAGE TemplateHaskell #-}
module Frost where

import PolysemyContrib
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error
import Polysemy.Trace
import Text.Pandoc.Extensions
import Text.Pandoc hiding (trace)
import Data.Map.Strict
import Data.Traversable

data DynamicError = DynamicError String deriving Show

generateDocs :: ( Member (Input Pandoc) r
                , Member (Output Pandoc) r
                , Member (Error DynamicError) r
                ) => (Pandoc -> Sem r (Either DynamicError Pandoc)) -> Sem r ()
generateDocs  transform = input >>= transform >>= either throw output

transform :: Pandoc -> Sem r (Either DynamicError Pandoc)
transform (Pandoc meta blocks) = do
  newMeta <- addToMeta defaultsMandatoryPlugin meta
  return $ Right (Pandoc newMeta blocks)

data Plugin r = Plugin {
                     addToMeta :: Meta -> Sem r Meta
                     }

plugins :: Member SystemEffect r => [Plugin r]
plugins = [timestampPlugin]

defaultsMandatoryPlugin :: Plugin r
defaultsMandatoryPlugin = Plugin atm
  where
    atm  = return . Meta . insertTitle . unMeta
    insertTitle = insert "title" (MetaString $ "Documentation")

timestampPlugin :: Member SystemEffect r => Plugin r
timestampPlugin = Plugin atm
  where
    atm :: Member SystemEffect r =>  Meta -> Sem r Meta
    atm meta = do
      time <- currentTime
      return $ Meta $ (insertTimestamp time) $ unMeta meta
    insertTimestamp t= insert "creation" (MetaString $ show t)

