{-# LANGUAGE QuasiQuotes #-}
module RunInputPandocSpec where

import           Frost.Effects.FileProvider
import           Frost.PandocRun

import           Polysemy
import           Polysemy.Error
import           Polysemy.Input
import           Polysemy.State
import           PolysemyContrib

import           Data.Function              ((&))
import           Data.Map
import qualified Data.Text                  as T
import           Test.Hspec
import           Text.Pandoc
import           Text.RawString.QQ

fetch :: String -> IO (Either PandocError Pandoc)
fetch content = do
  res <- input
    & runInputPandoc ["documentation.md"]
    & runFileProviderPure
    & runState (singleton "documentation.md" (T.pack content))
    & runError
    & runM
  return $ fmap (head . snd) res

pluginAsCodeBlock = [r|
```frost:plugin
```
|]

pluginAsCodeBlockWithContent = [r|
```frost:plugin
some content here
```
|]

pluginInlined = [r|
`frost:plugin`
|]

pluginInlinedSurroundedByText = [r|
The value is: `frost:plugin` ... wow!
|]

spec :: Spec
spec =
  describe "Frost.PandocRun runInputPandoc" $ do
    it "with plugin as code block" $ do
      Right(Pandoc _ blocks) <- fetch pluginAsCodeBlock
      blocks `shouldBe` [CodeBlock ("",["frost:plugin"],[]) ""]

    it "with plugin as code block with content" $ do
      Right(Pandoc _ blocks) <- fetch pluginAsCodeBlockWithContent
      blocks `shouldBe` [CodeBlock ("",["frost:plugin"],[]) "some content here"]

    it "with plugin as inlined code" $ do
      Right(Pandoc _ blocks) <- fetch pluginInlined
      blocks `shouldBe` [Para [Code ("",[],[]) "frost:plugin"]]

    it "with plugin as inlined code surrounded by text" $ do
      Right(Pandoc _ blocks) <- fetch pluginInlinedSurroundedByText
      blocks `shouldBe` [Para [ Str "The"
                              , Space
                              , Str "value"
                              , Space
                              , Str "is:"
                              , Space
                              , Code ("",[],[]) "frost:plugin"
                              , Space
                              , Str "..."
                              , Space
                              , Str "wow!"]]
