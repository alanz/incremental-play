module Language.Incremental.Visualise
 (
   Bar(..)
 , Span(..)
 , convert
 , asHierarchy
 ) where

import           Data.Tree
import qualified Language.Haskell.LSP.Types      as LSP
-- import qualified Language.Haskell.LSP.Types.Lens as LSP
-- import qualified Language.Haskell.LSP.Utility    as LSP
import           Language.Incremental.ParserTypes

import qualified Data.Text as T
-- import           Repetitive2
import           Repetitive3

-- ---------------------------------------------------------------------

data Bar = Bar { bLabel :: String
               , bToks :: [Tok]
               , bLength :: Int
               , bSpan :: Span
               } deriving Show

data Span = Span Int Int
            deriving Show

-- ---------------------------------------------------------------------

asHierarchy :: String -> [LSP.DocumentSymbol]
asHierarchy s = toHierarchy (bla s)

-- ---------------------------------------------------------------------

toHierarchy :: Tree Bar -> [LSP.DocumentSymbol]
toHierarchy (Node b []) = [mkDs (bLabel b) (bSpan b) Nothing]
toHierarchy (Node b ts) = [mkDs (bLabel b) (bSpan b) (Just children)]
  where
    children = LSP.List $ concatMap toHierarchy ts

mkDs :: String
          -> Span
          -> Maybe (LSP.List LSP.DocumentSymbol)
          -> LSP.DocumentSymbol
mkDs label (Span s e) children =
                 LSP.DocumentSymbol
                    (T.pack label)
                    Nothing
                    LSP.SkVariable
                    Nothing
                    sp
                    sp
                    children
  where
    sp = LSP.Range (LSP.Position 0 s) (LSP.Position 0 e)

-- ---------------------------------------------------------------------

-- bla :: String -> Tree Bar
bla s = pp $ fmap toBar (parse s)

-- Test stuff
-- ptree = (calc . lexer) "a BbDd c"
ptree = parse "a BbDd c"

parse :: String
           -> Tree
                (Val
                   (HappyAbsSyn Root () () Root Root () [B] B () (BinaryT B)) Tok)
parse s = (calc . mylexer) s

-- ---------------------------------------------------------------------

convert :: Show a => Tree (Val a Tok) -> Tree Bar
convert tree = pp $ fmap toBar tree

-- ---------------------------------------------------------------------

toBar :: (Show a) => Val a t -> (String, [t])
toBar v = (showConstr (here v), terminals v)


-- The Happy AST has each element we care about wrapped in a
-- constructor, we discard that first, together with the opening paren
showConstr :: (Show a) => a -> String
showConstr = fixup . head . tail . words . show
  where
    fixup "()" = "()"
    fixup ('(':r) = r
    fixup r = r

-- ---------------------------------------------------------------------

pp :: Tree (String,[Tok]) -> Tree Bar
pp t = head $ go (Span 0 0) [t]
  where
    go :: Span -> [Tree (String,[Tok])] -> [Tree Bar]
    go _ [] = []
    go sp@(Span _s e) (Node i []:tts) = Node b []:go sp' tts
      where
        b = (ff sp i) { bSpan = sp' }
        sp' = Span e (e + bLength b)
    go sp (Node i ts:tts) = r:go sp' tts
      where
        b = (ff sp i) { bSpan = sp' }
        ts' = go sp ts
        r = Node b ts'
        Node bs _ = head ts'
        Node be _ = last ts'
        Span s _ = bSpan bs
        Span _ e = bSpan be
        sp' = Span s e

    ff (Span _start end) (s,ts) = Bar s ts len sp
      where
        len = length ts
        sp = Span end (end + len)


-- ---------------------------------------------------------------------
