module VisualiseSpec (main, spec) where

import qualified Data.Text as T
import           Data.Tree
import           Data.List
import qualified Language.Haskell.LSP.Types as LSP
import           Language.Incremental.Lexer
import           Language.Incremental.LexerTypes
import           Language.Incremental.LexerTypes hiding (mkTok)
import           Language.Incremental.ParserTypes
import           Language.Incremental.Visualise
-- import           Repetitive3
import           Whitespace1
import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  hspec spec

-- ---------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Allocate Spans to tree" $ do
    it "tree trivial" $ do
      let
        -- str = " a\n B\n c"
        -- str = "a\n\nbBDd\n\nc\n"
        str = "a\n\nbBDd\n\nc\n"
        ptree = (calc . mylexer) str
        tree = convert ptree

      putStr $ drawTree $ fmap show tree
      putStrLn "---------------------------------------------"
      putStr $ intercalate "\n" $ show <$> treeToks tree
      putStrLn "\n---------------------------------------------"
      -- show tree `shouldBe` ""
      str `shouldBe` toksAsString (treeToks tree)


    -- ---------------------------------


-- ---------------------------------------------------------------------

mkTok :: String -> TokenL String
mkTok str
  = TokL
      { tokType      = T str
      , tokLexeme    = str
      , tokState     = 0
      , tokLookAhead = 0
      , tokLookBack  = 0
      , tokRelexed   = True
      }

-- ---------------------------------------------------------------------

addSentinels :: [TokenL s] -> [TokenL s]
addSentinels ts = bosToken : ts ++ [eosToken]
