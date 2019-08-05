module Main where

import Frost
import FrostError
import Frost.PandocRun (runInputPandoc, runOutputPandoc)
import Frost.Effects.FileProvider
import Frost.Effects.Git
import Frost.Effects.Sys

import Data.Function ((&))
import qualified Data.Text as T
import System.Exit (die, exitSuccess)
import Text.Pandoc (PandocError)
import Polysemy
import Polysemy.Error
import Polysemy.Trace
import PolysemyContrib

main :: IO ()
main =  generate >>= handleErrors
  where
    generate =  generateDocs (transform plugins)
      & runInputPandoc
      & runOutputPandoc
      & runFileProviderIO
      & runSysIO
      & runGitIO
      & runTraceIO
      & runError @DynamicError
      & runError @PandocError
      & runM
    handleErrors = either (die.show) (either (die.show) (\_ -> exitSuccess))
