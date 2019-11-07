module Main where

import Frost
import FrostError
import Frost.PandocRun (runInputPandoc, runOutputPandoc)
import Frost.Effects.FileProvider
import Frost.Effects.Git
import Frost.Effects.Python
import Frost.Effects.Rholang
import Frost.Effects.Sys
import Frost.Effects.Stack

import Data.Function ((&))
import qualified Data.Text as T
import System.Exit (die, exitSuccess)
import System.Environment (getArgs)
import Text.Pandoc (PandocError)
import Polysemy
import Polysemy.Error
import Polysemy.Trace
import PolysemyContrib

main :: IO ()
main = fmap head getArgs >>= generate >>= handleErrors
  where
    generate filePath = generateDocs (transform plugins)
      & runInputPandoc filePath
      & runOutputPandoc filePath
      & runFileProviderIO
      & runPython
      & runRholang
      & runStackSys
      & runSysIO
      & runGitIO
      & traceToIO
      & runError @FrostError
      & runError @PandocError
      & runM
    handleErrors = either (die.show) (either (die.show) (\_ -> exitSuccess))
