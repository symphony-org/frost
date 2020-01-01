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

import Data.Foldable (find)
import Data.Function ((&))
import qualified Data.Text as T
import System.Exit
import System.Environment (getArgs)
import System.IO
import Text.Pandoc (PandocError)
import Polysemy
import Polysemy.Error
import Polysemy.Trace
import PolysemyContrib

main :: IO ()
main = results >>= traverse handleEithers >>= exit
  where
    exit = maybe exitSuccess (\_ -> exitFailure) . find (== ExitFailure 1)
    results = getArgs >>= sequenceA . fmap generate
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
    handleEithers = either (handle) (either (handle) (const $ return ExitSuccess))
    handle error = hPutStrLn stderr (show error) >> return (ExitFailure 1)
