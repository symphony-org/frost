module Main where

import Frost
import Frost.Plugins (transform, plugins)

import Prelude hiding (readFile, writeFile)
import Data.Function ((&))
import qualified Data.Text as T
import System.Exit
import Text.Pandoc
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error
import Polysemy.Trace
import PolysemyContrib

main :: IO ()
main =  generate >>= handleErrors
  where
    generate =  generateDocs (transform plugins)
      & fetchInputFile
      & writeOutFile
      & runFileProviderIO
      & runSystemEffect
      & runTraceIO
      & runError @DynamicError
      & runError @PandocError
      & runM
    handleErrors = either (die.show) (either (die.show) (\_ -> exitSuccess))

fetchInputFile :: (
    Member (Lift IO) r
  , Member FileProvider r
  , Member (Error PandocError) r
  ) => Sem (Input Pandoc ': r) a -> Sem r a
fetchInputFile = interpret $ \case
  Input -> do
    content <- readFile "documentation.md"
    fromPandocIO $ readMarkdown settings content
  where
    settings = def { readerExtensions = extensionsFromList [Ext_yaml_metadata_block, Ext_backtick_code_blocks]}

writeOutFile :: (
    Member (Lift IO) r
  , Member FileProvider r
  , Member (Error PandocError) r
  ) => Sem (Output Pandoc ': r) a -> Sem r a
writeOutFile = interpret $ \case
  Output pandoc -> do
    content <- fromPandocIO $ writeHtml4String def pandoc
    writeFile "documentation.html" content

fromPandocIO :: (
    PandocMonad PandocIO
  , Member (Error PandocError) r
  , Member (Lift IO) r
  ) => PandocIO a -> Sem r a
fromPandocIO pioa = fromEitherSem $ sendM $ runIO pioa
