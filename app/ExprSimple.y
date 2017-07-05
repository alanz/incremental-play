{
-- This file is (initially) based on the example in happy manual
-- https://www.haskell.org/happy/doc/html/sec-using.html
module ExprSimple where

import Data.Char
import Data.List
import qualified Data.Bits as Bits
import Data.Text.Prettyprint.Doc
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

%%

-- -------------------------------------

-- This next section should become automatic, in time

Ultraroot : bos tree eos { $2 }

bos : { () }
eos : { () }

tree : Exp { $1 }

-- -------------------------------------

Exp   : Exp '+' Term            { Plus $1 $3 }
      | Exp '-' Term            { Minus $1 $3 }
      | Term                    { Term $1 }

Term  : Term '*' Factor         { Times $1 $3 }
      | Term '/' Factor         { Div $1 $3 }
      | Factor                  { Factor $1 }

Factor
      : int                     { Int $1 }


{
parseError :: [t] -> a
parseError _ = error "Parse error"

data Exp
      = Plus Exp Term
      | Minus Exp Term
      | Term Term
      deriving Show

data Term
      = Times Term Factor
      | Div Term Factor
      | Factor Factor
      deriving Show

data Factor
      = Int Int
      deriving Show

data Token
      = TokenInt Int
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenDiv
 deriving Show



lexer :: String -> [HappyInput]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isDigit c = lexNum (c:cs)
lexer ('+':cs) = InputToken TokenPlus : lexer cs
lexer ('-':cs) = InputToken TokenMinus : lexer cs
lexer ('*':cs) = InputToken TokenTimes : lexer cs
lexer ('/':cs) = InputToken TokenDiv : lexer cs
lexer (unk:cs) = error $ "lexer failure on char " ++ show unk

lexNum cs = InputToken (TokenInt (read num)) : lexer rest
      where (num,rest) = span isDigit cs

-- Main entry point. "calc" is the parser entry point generated above
/* main = getContents >>= print . calc . lexer */

}
