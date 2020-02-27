{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Frost.Effects.Thut where

import           Data.Text
import           Polysemy
import           Thut        as T
import           Thut.Render as T

data Thut m a where
  Eval :: Text -> Thut m String
  Passthrough :: Text -> Thut m String

makeSem ''Thut

runThutIO :: Member (Embed IO) r => Sem (Thut ': r) a -> Sem r a
runThutIO = interpret $ \case
  Eval contents -> embed $ process contents "eval"
  Passthrough contents -> embed $ process contents "passthrough"
  where
    filePath = "irrelevant"
    process contents mode = do
      doc <- T.evalText filePath ("```thut:" <> mode <> "\n" <> contents <> "\n```")
      rendered <- pure $ renderDocument doc
      pure $ unpack $ rendered
