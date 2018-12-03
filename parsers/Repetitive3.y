{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
module Repetitive3 where

import Data.Char
import Data.Foldable
import Data.Maybe
import Data.List
import Data.Tree
import qualified Data.Bits as Bits
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Language.Incremental.LexerTypes ( TokWrapper(..), TokenL(..) )
import qualified Language.Incremental.LexerTypes as LT
import BasicLexer

}

%name calc
%tokentype { TokenL TokenType }
%error { parseError }

-- %monad { P } { >>= } { return }
-- %lexer { lexer } { TokL { tokType = T EOF } }



-- The token section is a mapping from the shorthand we will use in this grammar
-- description to the token stracture provided by the lexer. The bit on the
-- right needs to make sense as a match clause in a haskell case statement.
%token
      'a'             { TokL { tokType = T TokenA  } }
      'b'             { TokL { tokType = T TokenBL } }
      'B'             { TokL { tokType = T TokenBU } }
      'd'             { TokL { tokType = T TokenBd } }
      'D'             { TokL { tokType = T TokenBD } }
      'c'             { TokL { tokType = T TokenC  } }

%%

-- -------------------------------------

-- This next section should become automatic, in time

Ultraroot : bos tree eos { $2 }

bos : { () }
eos : { () }

tree : Root { $1 }

-- -------------------------------------

Root : A Bs C { Root $2 }

A : 'a'           { () }
  | {- nothing -} { () }

Bs : listb(B)       { toList $1 }

B : 'b'            { BL }
  | 'B'            { BU }
  | 'd'            { Bd }
  | 'D'            { BD }

C : 'c'         { () }
  | {- nothing -} { () }

-- Rules to introduce a branching tree instead of linearity
-- See "Parameterized Productions" in
-- https://www.haskell.org/happy/doc/html/sec-grammar.html

list(p)         : list1(p)            { $1 }
                |                     { [] }

list1(p)        : rev_list1(p)        { reverse $1 }

rev_list1(p)    : p                   { [$1] }
                | rev_list1(p) p      { $2 : $1 }

-- -----------------------------

listb(p)         : lista(p)           { $1 }

lista(p)        : listb(p)            { $1 }
                |                     { BEmpty }

listb(p)        : p                   { BSingle $1 }
                | listb(p) listb(p)   { BDouble $1 $2 }

{-
s : A

A : B
   | {- nothing -}

B : L
   | B B
-}

-- ---------------------------------------------------------------------

{
parseError :: [t] -> a
parseError _ = error "Parse error"

data BinaryT a
  = BEmpty
  | BSingle a
  | BDouble (BinaryT a) (BinaryT a)
  deriving (Show, Foldable)

data Root = Root [B]
      deriving Show

data B = BL | BU | Bd | BD
     deriving Show

/* data Token */
/*       = TokenA */
/*       | TokenBL */
/*       | TokenBU */
/*       | TokenBd */
/*       | TokenBD */
/*       | TokenC */
/*  deriving Show */



-- lexer :: String -> [HappyInput]
lexerOrig str = [mkTokensNode (lexer' str)]

lexer' :: String -> [Tok]
lexer' [] = []
lexer' (c:cs)
      | isSpace c = lexer' cs
lexer' ('a':cs) = mt "a" TokenA  : lexer' cs
lexer' ('b':cs) = mt "b" TokenBL : lexer' cs
lexer' ('B':cs) = mt "B" TokenBU : lexer' cs
lexer' ('d':cs) = mt "d" TokenBd : lexer' cs
lexer' ('D':cs) = mt "D" TokenBD : lexer' cs
lexer' ('c':cs) = mt "c" TokenC  : lexer' cs
lexer' (unk:cs) = error $ "lexer' failure on char " ++ show unk

mt s t = mkTok (LT.mkTok s t)

-- Main entry point. "calc" is the parser entry point generated above
/* main = getContents >>= print . calc . lexer */

}
