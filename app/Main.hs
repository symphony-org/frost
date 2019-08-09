module Main where

import App
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
main =  mainString "documentation.md" >>= handleErrors
  where
    handleErrors = either (die.show) (either (die.show) (\_ -> exitSuccess))
