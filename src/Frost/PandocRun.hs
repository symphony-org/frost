module Frost.PandocRun where

import           Frost.Effects.FileProvider

import           Polysemy
import           Polysemy.Error
import           Polysemy.Input
import           Polysemy.Output
import           PolysemyContrib
import           Prelude                    hiding (readFile, writeFile)
import           Text.Pandoc

runInputPandoc :: (
    Member (Embed IO) r
  , Member FileProvider r
  , Member (Error PandocError) r
  ) => FilePath -> Sem (Input Pandoc ': r) a -> Sem r a
runInputPandoc filePath = interpret $ \case
  Input -> do
    content <- readFile filePath
    fromPandocIO $ readMarkdown settings content
  where
    settings = def { readerExtensions = extensionsFromList [Ext_yaml_metadata_block, Ext_backtick_code_blocks]}

runOutputPandoc :: (
    Member (Embed IO) r
  , Member FileProvider r
  , Member (Error PandocError) r
  ) => FilePath -> Sem (Output Pandoc ': r) a -> Sem r a
runOutputPandoc inputFilePath = interpret $ \case
  Output pandoc -> do
    content <- fromPandocIO $ writeHtml4String def pandoc
    writeFile (inputFilePath ++ ".html") content

fromPandocIO :: (
    PandocMonad PandocIO
  , Member (Error PandocError) r
  , Member (Embed IO) r
  ) => PandocIO a -> Sem r a
fromPandocIO pioa = fromEitherSem $ embed $ runIO pioa
