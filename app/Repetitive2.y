{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
module Repetitive2 where

import Data.Char
import Data.Foldable
import Data.Maybe
import Data.List
import Data.Tree
import qualified Data.Bits as Bits
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
}

%name calc
%tokentype { Token }
%error { parseError }

%token
      'a'             { TokenA }
      'b'             { TokenBL }
      'B'             { TokenBU }
      'd'             { TokenBd }
      'D'             { TokenBD }
      'c'             { TokenC }

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

data Token
      = TokenA
      | TokenBL
      | TokenBU
      | TokenBd
      | TokenBD
      | TokenC
 deriving Show



lexer :: String -> [HappyInput]
lexer str = [mkTokensNode (lexer' str)]

lexer' [] = []
lexer' (c:cs)
      | isSpace c = lexer' cs
lexer' ('a':cs) = mkTok TokenA  : lexer' cs
lexer' ('b':cs) = mkTok TokenBL : lexer' cs
lexer' ('B':cs) = mkTok TokenBU : lexer' cs
lexer' ('d':cs) = mkTok TokenBd : lexer' cs
lexer' ('D':cs) = mkTok TokenBD : lexer' cs
lexer' ('c':cs) = mkTok TokenC  : lexer' cs
lexer' (unk:cs) = error $ "lexer' failure on char " ++ show unk


-- Main entry point. "calc" is the parser entry point generated above
/* main = getContents >>= print . calc . lexer */

}
