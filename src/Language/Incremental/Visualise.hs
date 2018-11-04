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
import           Repetitive2

-- ---------------------------------------------------------------------

data Bar = Bar { bLabel :: String
               , bToks :: [Tok]
               , bLength :: Int
               , bSpan :: Span
               } deriving Show

data Span = Span Int Int
            deriving Show

-- ---------------------------------------------------------------------

asHierarchy :: [LSP.DocumentSymbol]
asHierarchy = toHierarchy bla

-- ---------------------------------------------------------------------

toHierarchy :: Tree Bar -> [LSP.DocumentSymbol]
toHierarchy (Node b []) = [mkDs (bLabel b) (bSpan b) Nothing]
toHierarchy (Node b ts) = [mkDs (bLabel b) (bSpan b) (Just children)]
  where
    children = LSP.List $ concatMap toHierarchy ts

mkDs label span children =
                 LSP.DocumentSymbol
                    (T.pack label)
                    Nothing
                    LSP.SkVariable
                    Nothing
                    (LSP.Range (LSP.Position 0 0) (LSP.Position 0 3))
                    (LSP.Range (LSP.Position 0 0) (LSP.Position 0 3))
                    children

-- ---------------------------------------------------------------------

bla :: Tree Bar
bla = pp $ fmap toBar ptree

-- Test stuff
ptree = (calc . lexer) "a BbDd c"

-- ---------------------------------------------------------------------

convert :: Show a => Tree (Val a Tok) -> Tree Bar
convert ptree = pp $ fmap toBar ptree

-- ---------------------------------------------------------------------

toBar :: (Show a) => Val a t -> (String, [t])
toBar v = (show (here v), terminals v)

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
