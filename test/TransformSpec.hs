{-# LANGUAGE QuasiQuotes #-}
module TransformSpec where

import Frost
import Frost.Plugin
import Frost.Plugins (transform)

import Text.Pandoc
import Data.Function ((&))
import Polysemy
import Polysemy.Input
import Polysemy.Output
import qualified Data.Text as T
import Test.Hspec
import Text.RawString.QQ


purgePlugin :: Plugin r
purgePlugin = Plugin { pluginName = "null"
                    , substitute = \_ -> return []
                    , addToMeta = \m -> return nullMeta
                    }

textPlugin :: String -> Plugin r
textPlugin text= Plugin { pluginName = "text:insert"
                    , substitute = \_ -> return [Plain [Str text]]
                    , addToMeta = \m -> return nullMeta
                    }

doublePlugin ::  Plugin r
doublePlugin= Plugin { pluginName = "double"
                    , substitute = \i -> return [Plain [Str $ show $ 2 * read i]]
                    , addToMeta = \m -> return nullMeta
                    }

spec :: Spec
spec =
  describe "Frost.Plugins transform" $ do
    it "should keep document as is, if no frost code blocks in it" $ do
      -- given
      let blocks = [ HorizontalRule
                   , Plain [Str "test"]]
      let pandoc = Pandoc nullMeta blocks
      -- when
      let Right(transformed) =  run $ transform [purgePlugin] pandoc
      -- then
      transformed `shouldBe` pandoc

    it "should substitute frost code blocks with content from plugin" $ do
      -- given
      let blocks = [ HorizontalRule
                   , CodeBlock ("",["frost:text:insert"],[]) ""]
      let pandoc = Pandoc nullMeta blocks
      -- when
      let Right(Pandoc _ transformedBlocks) =  run $ transform [textPlugin "hello world!"] pandoc
      -- then
      transformedBlocks `shouldBe` [ HorizontalRule
                                   , Plain [Str "hello world!"]]

    it "should modify document with multiple plugins" $ do
      -- given
      let blocks = [ HorizontalRule
                  , CodeBlock ("",["frost:text:insert"],[]) ""
                  , CodeBlock ("",["frost:double"],[]) "2"]
      let pandoc = Pandoc nullMeta blocks
      let plugs = [doublePlugin, textPlugin "hello world!"]
      -- when
      let Right(Pandoc _ transformedBlocks) =  run $ transform plugs pandoc
      -- then
      transformedBlocks `shouldBe` [ HorizontalRule
                                   , Plain [Str "hello world!"]
                                   , Plain [Str "4"]]

