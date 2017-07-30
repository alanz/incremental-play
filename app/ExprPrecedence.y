{
-- This file is (initially) based on the example in happy manual
-- https://www.haskell.org/happy/doc/html/sec-using.html
{-# LANGUAGE OverloadedStrings #-}
module ExprSimple where

import Data.Char
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
      int             { TokenInt $$ }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDiv }

%left '+' '-'
%left '*' '/'

%%

-- -------------------------------------

-- This next section should become automatic, in time

Ultraroot : bos tree eos { $2 }

bos : { () }
eos : { () }

tree : Exp { $1 }

-- -------------------------------------

Exp   : Exp '+' Exp            { Plus $1 $3 }
      | Exp '-' Exp            { Minus $1 $3 }
      | Exp '*' Exp            { Times $1 $3 }
      | Exp '/' Exp            { Div $1 $3 }
      | int                    { Int $1 }


{
parseError :: [t] -> a
parseError _ = error "Parse error"

data Exp
      = Plus Exp Exp
      | Minus Exp Exp
      | Times Exp Exp
      | Div Exp Exp
      | Int Int
      deriving Show

data Token
      = TokenInt Int
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenDiv
 deriving Show



lexer :: String -> [HappyInput]
lexer str = [mkTokensNode (lexer' str)]

lexer' [] = []
lexer' (c:cs)
      | isSpace c = lexer' cs
      | isDigit c = lexNum (c:cs)
lexer' ('+':cs) = mkTok TokenPlus : lexer' cs
lexer' ('-':cs) = mkTok TokenMinus : lexer' cs
lexer' ('*':cs) = mkTok TokenTimes : lexer' cs
lexer' ('/':cs) = mkTok TokenDiv : lexer' cs
lexer' (unk:cs) = error $ "lexer' failure on char " ++ show unk

lexNum cs = mkTok (TokenInt (read num)) : lexer' rest
      where (num,rest) = span isDigit cs

-- Main entry point. "calc" is the parser entry point generated above
/* main = getContents >>= print . calc . lexer */

}
