module Main where

import           Frost
import           Frost.DefaultsMandatoryPlugin
import           Frost.Effects.FileProvider
import           Frost.Effects.Git
import           Frost.Effects.Python
import           Frost.Effects.Rholang
import           Frost.Effects.Stack
import           Frost.Effects.Sys
import           Frost.Effects.Thut
import           Frost.PandocRun                     (runInputPandoc,
                                                      runOutputPandoc)
import           Frost.Plugin
import           Frost.Plugins.GitContributorsPlugin
import           Frost.Plugins.RholangPlugin
import           Frost.Plugins.StackPlugins
import           Frost.Plugins.ThutPlugin
import           Frost.PythonPlugin
import           Frost.TimestampPlugin
import           FrostError

import           Data.Foldable                       (find)
import           Data.Function                       ((&))
import qualified Data.Text                           as T
import           Polysemy
import           Polysemy.Error
import           Polysemy.Trace
import           PolysemyContrib
import           System.Environment                  (getArgs)
import           System.Exit
import           System.IO
import           Text.Pandoc                         (PandocError)

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
      & runThutIO
      & runSysIO
      & runGitIO
      & traceToIO
      & runError @FrostError
      & runError @PandocError
      & runM
    handleEithers = either (handle) (either (handle) (const $ return ExitSuccess))
    handle error = hPutStrLn stderr (show error) >> return (ExitFailure 1)

plugins :: Members [Git, Python, Rholang, Sys, Stack, Thut] r  => [Plugin r]
plugins = [ timestampPlugin
          , timestampMetaPlugin
          , defaultsMandatoryPlugin
          , gitContributorsPlugin
          , pythonPlugin
          , rholangPlugin
          ] ++ stackPlugins ++ thutPlugins
