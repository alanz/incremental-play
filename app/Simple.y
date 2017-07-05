{
-- This file is (initially) based on the example in happy manual
-- https://www.haskell.org/happy/doc/html/sec-using.html
module Simple where

import Data.Char
import Data.List
import qualified Data.Bits as Bits
}

%name calc
%tokentype { Token }
%error { parseError }

%token
      'A'             { TokenA }
      'B'             { TokenB }
      'C'             { TokenC }

%%

Ultraroot : bos tree eos { $2 }

bos : { () }
eos : { () }

tree : Top { $1 }

Top   : 'A' 'B'      { Top TASingle TB }
      | 'A' 'A' 'B'  { Top TADouble TB }



{
parseError :: [t] -> a
parseError _ = error "Parse error"

-- -------------------------------------

data Top
      = Top TA TB
      deriving Show

data TA = TASingle | TADouble
      deriving Show

data TB = TB
  deriving Show


-- -------------------------------------

data Token
      = TokenA
      | TokenB
      | TokenC
 deriving Show

lexer :: String -> [HappyInput]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
lexer ('A':cs) = InputToken TokenA : lexer cs
lexer ('B':cs) = InputToken TokenB : lexer cs
lexer ('C':cs) = InputToken TokenC : lexer cs


-- Main entry point
/* main = getContents >>= print . calc . lexer */

}
