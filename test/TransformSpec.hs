module TransformSpec where

import Frost
import FrostError
import Frost.Plugin

import Text.Pandoc
import Data.Map
import Data.Function ((&))
import Polysemy
import Polysemy.Error
import qualified Data.Text as T
import Test.Hspec

purgePlugin :: Plugin r
purgePlugin = Plugin { pluginName = "null"
                     , substitute = \_ -> return []
                     , addToMeta = \m -> return nullMeta
                     }

textPlugin :: String -> Plugin r
textPlugin text = justContentPlugin "text:insert" (\_ -> return [Plain [Str text]])

doublePlugin ::  Plugin r
doublePlugin= justContentPlugin "double" (\i -> return [Plain [Str $ show $ 2 * read i]])

addEntryMetaPlugin :: String -> String -> Plugin r
addEntryMetaPlugin key value = justMetaPlugin "meta:plugin" (\meta -> return $ Meta (insert key (MetaString value) ( unMeta meta) ) )

spec :: Spec
spec =
  describe "Frost.Plugins transform" $ do
    it "should keep document as is, if no frost code blocks in it" $ do
      -- given
      let blocks = [ HorizontalRule
                   , Plain [Str "test"]]
      let pandoc = Pandoc nullMeta blocks
      -- when
      let Right(transformed) =  run $ runError $ transform [purgePlugin] pandoc
      -- then
      transformed `shouldBe` pandoc

    it "should substitute frost code blocks with content from plugin" $ do
      -- given
      let blocks = [ CodeBlock ("",["frost:text:insert"],[]) ""]
      let pandoc = Pandoc nullMeta blocks
      -- when
      let Right(Pandoc _ transformedBlocks) =
            run $ runError $ transform [textPlugin "hello world!"] pandoc
      -- then
      transformedBlocks `shouldBe` [ Plain [Str "hello world!"]]

    it "should modify document with multiple plugins" $ do
      -- given
      let blocks = [ CodeBlock ("",["frost:text:insert"],[]) ""
                   , CodeBlock ("",["frost:double"],[]) "2"]
      let pandoc = Pandoc nullMeta blocks
      let plugs = [doublePlugin, textPlugin "hello world!"]
      -- when
      let Right(Pandoc _ transformedBlocks) =  run $ runError $ transform plugs pandoc
      -- then
      transformedBlocks `shouldBe` [ Plain [Str "hello world!"]
                                   , Plain [Str "4"]]

    it "should modify a document with multiple meta plugins" $ do
      -- given
      let pandoc = Pandoc nullMeta [Null]
      let plugs = [addEntryMetaPlugin "key1" "value1", addEntryMetaPlugin "key2" "value2"]
      -- when
      let Right(Pandoc transformedMeta _) =  run $ runError $ transform plugs pandoc
      -- then
      transformedMeta `shouldBe` Meta (fromList [("key1", MetaString("value1")),("key2", MetaString("value2"))])

    it "should stop with error if plugin not found for given frost block" $ do
      -- given
      let blocks = [CodeBlock ("",["frost:text:insert"],[]) ""]
      let pandoc = Pandoc nullMeta blocks
      let plugs = [purgePlugin]
      -- when
      let Left(error) =  run $ runError $ transform plugs pandoc
      -- then
      error `shouldBe` PluginNotAvailable "frost:text:insert"
