{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Frost.Effects.Thut where

import           Data.Text
import           Polysemy
import           Thut        as T
import           Thut.Render as T

data Thut m a where
  Eval :: Text -> Thut m Text 
  Passthrough :: Text -> Thut m Text 

makeSem ''Thut

runThutIO :: Member (Embed IO) r => Sem (Thut ': r) a -> Sem r a
runThutIO = interpret $ \case
  Eval contents -> embed $ process contents "eval"
  Passthrough contents -> embed $ process contents "passthrough"
  where
    filePath = "irrelevant"
    process contents mode = do
      doc <- T.evalText' config filePath ("```thut:" <> mode <> "\n" <> contents <> "\n```")
      pure $ renderDocument doc
    config = InterpreterConfig "stack repl" False Plain verboseGhcid
    verboseGhcid = False
