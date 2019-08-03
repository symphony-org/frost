module Main where

import Frost
import Frost.PandocRun (runInputPandoc, runOutputPandoc)

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
      & runSystemEffect
      & runTraceIO
      & runError @DynamicError
      & runError @PandocError
      & runM
    handleErrors = either (die.show) (either (die.show) (\_ -> exitSuccess))


