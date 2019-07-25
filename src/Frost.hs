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

runDynamicContent :: (
    Member Trace r
  , Member SystemEffect r
  ) => Sem (DynamicContent ': r) a -> Sem r a
runDynamicContent = interpret $ \case
  Transform (Pandoc meta blocks) -> do
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

