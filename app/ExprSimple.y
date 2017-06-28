{
-- This file is (initially) based on the example in happy manual
-- https://www.haskell.org/happy/doc/html/sec-using.html
module Main where
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

Exp   : Exp '+' Term           { Plus $1 $3 }
      | Exp '-' Term           { Minus $1 $3 }
      | Term                    { Term $1 }

Term  : Term '*' Factor         { Times $1 $3 }
      | Term '/' Factor         { Div $1 $3 }
      | Factor                  { Factor $1 }

Factor
      : int                     { Int $1 }


{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp
      = Plus Exp Term
      | Minus Exp Term
      | Term Term
      deriving Show

data Term
      = Times Term Factor
      | Div Term Factor
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

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isDigit c = lexNum (c:cs)
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

-- Main entry point. "calc" is the parser entry point generated above
main = getContents >>= print . calc . lexer

}
