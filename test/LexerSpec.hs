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
      fixLookBacks' toks `shouldBe` addSentinels toks

    -- ---------------------------------

    it "calc lookback single" $ do
      let
        toks =  [(mkTok "a") { tokLookAhead = 1 }, mkTok "bbb",                     mkTok "c"]
        toks' = [(mkTok "a") { tokLookAhead = 1 },(mkTok "bbb") { tokLookBack = 1 },mkTok "c"]
      fixLookBacks' toks `shouldBe` addSentinels toks'

    -- ---------------------------------

    it "calc lookback double" $ do
      let
        toks =  [(mkTok "a") { tokLookAhead = 4 }, mkTok "bbb",                     mkTok "c"]
        toks' = [(mkTok "a") { tokLookAhead = 4 },(mkTok "bbb") { tokLookBack = 1 },(mkTok "c") { tokLookBack = 2 }]
      fixLookBacks' toks `shouldBe` addSentinels toks'

-- ---------------------------------------------------------------------

mkTok :: String -> Token String
mkTok str
  = Tok
      { tokType      = T str
      , tokLexeme    = str
      , tokState     = 0
      , tokLookAhead = 0
      , tokLookBack  = 0
      , tokRelexed   = True
      }

-- ---------------------------------------------------------------------

addSentinels :: [Token s] -> [Token s]
addSentinels ts = bosToken : ts ++ [eosToken]
