{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
module Whitespace1 where

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
      'WS'            { TokL { tokType = T WS      } }
      'BOF'           { TokL { tokType = T BOF     } }

%%

-- -------------------------------------

-- This next section should become automatic, in time

Ultraroot : bos tree eos { $2 }

bos : conn('BOF') { Conn "bos" }
eos : { "eos" }

tree : Root { $1 }

-- -------------------------------------

Root : Am Bs Cm { Root $2 }

Am :: { (Conn (TokenL TokenType)) }
   : conn(A)      { $1 }

A : 'a' { $1 }

Bs :: { [ Conn B]}
   : listb(BWs)       { toList $1 }

BWs :: { Conn B }
    : conn(B)         { $1 }

B :: { B }
  : 'b'            { BL }
  | 'B'            { BU }
  | 'd'            { Bd }
  | 'D'            { BD }

Cm :: { (Conn (TokenL TokenType)) }
   : conn(C)      { $1 }

C : 'c'           { $1 }

-- Rules to introduce a branching tree instead of linearity
-- See "Parameterized Productions" in
-- https://www.haskell.org/happy/doc/html/sec-grammar.html

list(p)         : list1(p)            { $1 }
                |                     { [] }

list1(p)        : rev_list1(p)        { reverse $1 }

rev_list1(p)    : p                   { [$1] }
                | rev_list1(p) p      { $2 : $1 }

-- -----------------------------

listb(p)        : lista(p)            { $1 }

lista(p)        : listb(p)            { $1 }
                |                     { BEmpty }

listb(p)        : p                   { BSingle $1 }
                | listb(p) listb(p)   { BDouble $1 $2 }

-- -----------------------------

-- Rules for managing whitespace, explicitly
-- Pair productions are shared
-- by all whitespace sequences.
ws_pair : 'WS'    'WS' { PairWW }
  | 'WS'    ws_pair    { PairWP $1 $2 }
  | ws_pair 'WS'       { PairPW $1 $2 }
  | ws_pair ws_pair    { PairPP $1 $2 }

conn(id) : id               { Conn  $1 }
         | conn_pair(id)    { $1 }
conn_pair(id) : id 'WS'     { ConnW $1 }
              | id ws_pair  { ConnP $1 $2 }

-- -----------------------------
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

-- ---------------------------------------------------------------------
-- whitespace management

data Conn a
  = ConnW a
  | ConnP a Pair
  | Conn  a
  deriving (Show, Foldable)

data Pair
  = PairWW
  | PairWP WS Pair
  | PairPW Pair WS
  | PairPP Pair Pair
  deriving (Show)

type WS = TokenL TokenType

-- ---------------------------------------------------------------------
data Root = Root [Conn B]
      deriving Show

data B = BL | BU | Bd | BD
     deriving Show

{-
data Token
      = TokenA
      | TokenBL
      | TokenBU
      | TokenBd
      | TokenBD
      | TokenC
 deriving Show
-}

{-
mylexer :: String
  -> [Node (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13) Tok] -}
mylexer s = [mkTokensNode toks]
  where
  toks = mkTok alexBOF : lll s

lll :: String -> [Tok]
lll s =
  case runAlex s (lexer cc) of
    Left err -> error err
    Right v  -> v
  where
    cc :: LT.TokenL TokenType -> Alex [Tok]
    cc ltok = case LT.tokType ltok of
      LT.T EOF -> return []
      -- LT.T WS  -> lexer cc
      _        -> (mkTok ltok :) <$> lexer cc


-- lexer :: String -> [HappyInput]

-- Main entry point. "calc" is the parser entry point generated above
-- main = getContents >>= print . calc . lexer

}
