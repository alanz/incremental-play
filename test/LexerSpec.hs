module LexerSpec (main, spec) where

import           Test.Hspec
import           Language.Incremental.Lexer
import           Language.Incremental.LexerTypes

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  hspec spec

-- ---------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Calculate lookbacks" $ do
    it "calc lookback trivial" $ do
      let
        toks = [mkTok "a",mkTok "b",mkTok "c"]
      fixLookBacks' toks `shouldBe` toks

    -- ---------------------------------

    it "calc lookback single" $ do
      let
        toks = [(mkTok "a") { tokLookAhead = 1 },mkTok "bbb",mkTok "c"]
        toks' = [(mkTok "a") { tokLookAhead = 1 },(mkTok "bbb") { tokLookBack = 1 },mkTok "c"]
      fixLookBacks' toks `shouldBe` toks'

-- ---------------------------------------------------------------------

mkTok :: String -> Token String
mkTok str = Tok str str 0 0 0
