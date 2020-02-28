module Frost.PandocRun where

import           Frost.Effects.FileProvider

import           Data.Text                  (unpack)
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
  ) => [FilePath] -> Sem (Input [Pandoc] ': r) a -> Sem r a
runInputPandoc filePaths = interpret $ \case
  Input -> do
    contents <- traverse readFile filePaths
    traverse (fromPandocIO . readMarkdown settings) contents
  where
    settings = def { readerExtensions = extensionsFromList [Ext_yaml_metadata_block, Ext_backtick_code_blocks]}

runOutputPandoc :: (
    Member (Embed IO) r
  , Member FileProvider r
  , Member (Error PandocError) r
  ) => FilePath -> FilePath -> Sem (Output Pandoc ': r) a -> Sem r a
runOutputPandoc inputFilePath templatePath = interpret $ \case
  Output pandoc -> do
    template <- readFile templatePath
    let options = mkOptions $ unpack template
    content <- fromPandocIO $ writeHtml4String options pandoc
    writeFile inputFilePath content
  where
    mkOptions template = def
      { writerTableOfContents = True
      , writerTemplate = Just template
      }

fromPandocIO :: (
    PandocMonad PandocIO
  , Member (Error PandocError) r
  , Member (Embed IO) r
  ) => PandocIO a -> Sem r a
fromPandocIO pioa = fromEitherSem $ embed $ runIO pioa
