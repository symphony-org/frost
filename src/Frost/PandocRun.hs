module Frost.PandocRun where

import Frost.Effects.FileProvider

import Prelude hiding (readFile, writeFile)
import Text.Pandoc hiding (trace)
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error
import PolysemyContrib

runInputPandoc :: (
    Member (Embed IO) r
  , Member FileProvider r
  , Member (Error PandocError) r
  ) => Sem (Input Pandoc ': r) a -> Sem r a
runInputPandoc = interpret $ \case
  Input -> do
    content <- readFile "documentation.md"
    fromPandocIO $ readMarkdown settings content
  where
    settings = def { readerExtensions = extensionsFromList [Ext_yaml_metadata_block, Ext_backtick_code_blocks]}

runOutputPandoc :: (
    Member (Embed IO) r
  , Member FileProvider r
  , Member (Error PandocError) r
  ) => Sem (Output Pandoc ': r) a -> Sem r a
runOutputPandoc = interpret $ \case
  Output pandoc -> do
    content <- fromPandocIO $ writeHtml4String def pandoc
    writeFile "documentation.html" content

fromPandocIO :: (
    PandocMonad PandocIO
  , Member (Error PandocError) r
  , Member (Embed IO) r
  ) => PandocIO a -> Sem r a
fromPandocIO pioa = fromEitherSem $ embed $ runIO pioa
