module Main where

import Frost

import Data.Functor
import Text.Pandoc
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error

main :: IO ()
main =  generate >>= handleErrors
  where
    generate =  runM  $ runError $ runDynamicContent $ fetchInputFile $  fetchOutFile $ generateDocs
    handleErrors = either (putStrLn.show) (\_ -> return ())

fetchInputFile :: Sem (Input Pandoc ': r) a -> Sem r a
fetchInputFile = undefined

fetchOutFile :: Sem (Output Pandoc ': r) a -> Sem r a
fetchOutFile = undefined
