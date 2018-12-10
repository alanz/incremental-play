module Language.Incremental.Visualise
 (
   Bar(..)
 , Span(..)
 , Pos(..)
 , convert
 , asHierarchy
 , treeToks
 , toksAsString
 ) where

import           Data.Foldable
import           Data.List
import qualified Data.Text as T
import           Data.Tree
import qualified Language.Haskell.LSP.Types as LSP
import           Language.Incremental.LexerTypes
import           Language.Incremental.ParserTypes
-- import           Repetitive3
import           Whitespace1

-- ---------------------------------------------------------------------

data Bar = Bar { bLabel  :: String -- ^ Of the Constructor for this node
               , bToks   :: [Tok]  -- ^ The tokens parsed to get this
               , bLength :: Int    -- ^ Length in tokens?
               , bSpan   :: Span   -- ^ Area the tokens cover, including embedded newlines
               }
instance Show Bar where
  show (Bar label toks len sp)
    = parens $ unwords ["Bar", show label, show toks, show len, show sp]

data Span = Span Pos Pos

instance Show Span where
  show (Span f t) = parens $ unwords ["Span",show f,show t]

data Pos = Pos { line :: Int
               , col :: Int
               }
instance Show Pos where
  show (Pos f t) = parens $ unwords ["Pos",show f,show t]

parens str = "(" ++ str ++ ")"

-- ---------------------------------------------------------------------

asHierarchy :: String -> [LSP.DocumentSymbol]
asHierarchy s = toHierarchy (bla s)

-- ---------------------------------------------------------------------

treeToks :: Tree Bar -> [Tok]
treeToks t = fold $ fmap bToks t

toksAsString :: [Tok] -> String
toksAsString ts = concatMap foo ts
  where
    foo (Tok _ t) = tokLexeme t

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
mkDs label (Span (Pos sl sc) (Pos el ec)) children =
                 LSP.DocumentSymbol
                    (T.pack label)
                    Nothing
                    LSP.SkVariable
                    Nothing
                    sp
                    sp
                    children
  where
    sp = LSP.Range (LSP.Position sl sc) (LSP.Position el ec)

-- ---------------------------------------------------------------------

bla :: String -> Tree Bar
bla s = pp $ fmap toBar (parse s)

-- Test stuff
-- ptree = (calc . lexer) "a BbDd c"
ptree = parse "a BbDd c"

-- parse :: String
--            -> Tree
--                 (Val
--                    (HappyAbsSyn Root () () Root Root () [B] B () (BinaryT B)) Tok)
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

-- Add locations, based on the raw tokens
pp :: Tree (String,[Tok]) -> Tree Bar
pp t = head $ go (Span (Pos 0 0) (Pos 0 0)) [t]
  where
    go :: Span -> [Tree (String,[Tok])] -> [Tree Bar]
    go _ [] = []
    go sp@(Span (Pos sl _s) (Pos el e)) (Node i []:tts) = Node b []:go (bSpan b) tts
      where
        b = (ff sp i)
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

    -- Given a starting span, advance to calculate a Span
    -- corresponding to the given tokens
    ff :: Span -> (String,[Tok]) -> Bar
    ff (Span _start end@(Pos endl endc)) (s,ts) = Bar s ts len sp
      where
        len = length ts
        -- sp = Span (Pos endl endc) (Pos endl (endc + len))
        sp = Span start' end'
        -- start' = Pos endl (endc+1)
        start' = Pos endl endc
        end' = go1 end ts

        go1 pos [] = pos
        go1 pos (Tok _ t':ts') = go1 (advance pos t') ts'

    -- Advance based on the lexeme text, adjusting for any newlines seen
    advance :: Pos -> TokenL t -> Pos
    advance pos tok = go2 pos (tokLexeme tok)
      where
        go2 pos' [] = pos'
        go2 (Pos l _) ('\n':cs) = go2 (Pos (l+1)     0) cs
        go2 (Pos l c) (   _:cs) = go2 (Pos  l    (c+1)) cs


-- ---------------------------------------------------------------------
