module App where

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

mainString :: String -> IO (Either PandocError (Either FrostError ())) --String
mainString filePath = generate -- >>= handleErrors
  where
    generate =  generateDocs (transform plugins)
      & (runInputPandoc filePath)
      & runOutputPandoc
      & runFileProviderIO
      & runSysIO
      & runGitIO
      & traceToIO
      & runError @FrostError
      & runError @PandocError
      & runM
