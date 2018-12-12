module Language.Incremental.Visualise
 (
   Bar(..)
 , Range(..)
 , Position(..)
 , parseTree
 , convert
 , asHierarchy
 , toHierarchy
 , treeToks
 , toksAsString
 , asLocMap
 , getArtifactsAtPos
 ) where

import           Data.Foldable
import qualified Data.Text as T
import           Data.Tree
import qualified Language.Haskell.LSP.Types as LSP
import           Language.Incremental.LexerTypes
import           Language.Incremental.ParserTypes
import qualified Data.IntervalMap.FingerTree       as IM
-- import           Repetitive3
import           Whitespace1
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types.Lens
import           Control.Lens ( (^.), (.~) )


-- ---------------------------------------------------------------------

data Bar = Bar { bLabel  :: String -- ^ Of the Constructor for this node
               , bToks   :: [Tok]  -- ^ The tokens parsed to get this
               , bLength :: Int    -- ^ Length in tokens?
               , bSpan   :: Range  -- ^ Area the tokens cover, including embedded newlines
               }
instance Show Bar where
  show (Bar label toks len sp)
    = parens $ unwords ["Bar", show label, show toks, show len, show sp]

-- data Span = Span Pos Pos

-- instance Show Span where
--   show (Span f t) = parens $ unwords ["Span",show f,show t]

-- data Pos = Pos { line :: Int
--                , col :: Int
--                }
-- instance Show Pos where
--   show (Pos f t) = parens $ unwords ["Pos",show f,show t]

parens :: String -> String
parens str = "(" ++ str ++ ")"

-- ---------------------------------------------------------------------

asHierarchy :: String -> [LSP.DocumentSymbol]
asHierarchy s = toHierarchy (parseTree s)

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
          -> Range
          -> Maybe (LSP.List LSP.DocumentSymbol)
          -> LSP.DocumentSymbol
mkDs label sp children =
                 LSP.DocumentSymbol
                    (T.pack label)
                    Nothing
                    LSP.SkVariable
                    Nothing
                    sp
                    sp
                    children

-- ---------------------------------------------------------------------

parseTree :: String -> Tree Bar
parseTree s = toLocatedTree $ fmap toBar (parse s)
  where
    -- parse :: String
    --            -> Tree
    --                 (Val
    --                    (HappyAbsSyn Root () () Root Root () [B] B () (BinaryT B)) Tok)
    parse s' = (calc . mylexer) s'

-- Test stuff
-- ptree = (calc . lexer) "a BbDd c"
-- ptree = parse "a BbDd c"


-- ---------------------------------------------------------------------

convert :: Show a => Tree (Val a Tok) -> Tree Bar
convert tree = toLocatedTree $ fmap toBar tree

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
toLocatedTree :: Tree (String,[Tok]) -> Tree Bar
toLocatedTree t = head $ go (Range (Position 0 0) (Position 0 0)) [t]
  where
    go :: Range -> [Tree (String,[Tok])] -> [Tree Bar]
    go _ [] = []
    go sp (Node i []:tts) = Node b []:go (bSpan b) tts
      where
        b = ff sp i
    go sp (Node i ts:tts) = r:go sp' tts
      where
        b = (ff sp i) { bSpan = sp' }
        ts' = go sp ts
        r = Node b ts'
        Node bs _ = head ts'
        Node be _ = last ts'
        Range s _ = bSpan bs
        Range _ e = bSpan be
        sp' = Range s e

    -- Given a starting span, advance to calculate a Span
    -- corresponding to the given tokens
    ff :: Range -> (String,[Tok]) -> Bar
    ff (Range _start end) (s,ts) = Bar s ts len sp
      where
        len    = length ts
        sp     = Range end end'
        end'   = go1 end ts

        go1 pos [] = pos
        go1 pos (Tok _ t':ts') = go1 (advance pos t') ts'

    -- Advance based on the lexeme text, adjusting for any newlines seen
    advance :: Position -> TokenL t -> Position
    advance pos tok = go2 pos (tokLexeme tok)
      where
        go2 pos' [] = pos'
        go2 (Position l _) ('\n':cs) = go2 (Position (l+1)     0) cs
        go2 (Position l c) (   _:cs) = go2 (Position  l    (c+1)) cs


-- ---------------------------------------------------------------------

type SourceMap a = IM.IntervalMap Position a
type AstMap      = SourceMap T.Text

-- ---------------------------------------------------------------------

getArtifactsAtPos :: Position -> SourceMap a -> [(Range, a)]
getArtifactsAtPos p im = map f $ IM.search p im
  where f (IM.Interval a b, x) = (Range a b, x)

-- ---------------------------------------------------------------------

genIntervalMap :: [(Position,Position,a)] -> SourceMap a
genIntervalMap ts = foldr go IM.empty ts
  where
    go (l,h,x) im = IM.insert (IM.Interval l h) x im

-- ---------------------------------------------------------------------

asLocMap :: Tree Bar -> AstMap
asLocMap tree = r
  where
    h = toHierarchy tree
    go :: [DocumentSymbol] -> [(Position,Position,T.Text)]
    go [] = []
    go (ds:dss) = (fm,to,nm) : go (dss ++ kids')
      where
        Range fm to = ds ^. range
        kids        = ds ^. children
        nm          = ds ^. name
        kids' = case kids of
          Nothing -> []
          Just (List ls) -> ls
    intervals = go h
    r = genIntervalMap intervals

-- ---------------------------------------------------------------------
