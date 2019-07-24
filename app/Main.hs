module Main where

import Frost

import Prelude hiding (readFile, writeFile)
import Data.Function
import qualified Data.Text as T
import System.Exit
import Text.Pandoc
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error
import PolysemyContrib

main :: IO ()
main =  generate >>= handleErrors
  where
    generate =  generateDocs
      & fetchInputFile
      & writeOutFile
      & runFileProviderIO
      & runDynamicContent
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
    fromEitherSem $ (sendM $ runIO $ readMarkdown def content)

writeOutFile :: Sem (Output Pandoc ': r) a -> Sem r a
writeOutFile = undefined

