{
-- This file is (initially) based on the example in happy manual
-- https://www.haskell.org/happy/doc/html/sec-using.html
module Simple where

import Data.Char
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

bos : { undefined }
eos : { undefined }

tree : Top { $1 }

Top   : 'A' 'B'  { Top TA TB }



{
parseError :: [Token] -> a
parseError _ = error "Parse error"

-- -------------------------------------

data Top
      = Top TA TB
      deriving Show

data TA = TA
      deriving Show

data TB = TB
  deriving Show


-- -------------------------------------

data Token
      = TokenA
      | TokenB
      | TokenC
 deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
lexer ('A':cs) = TokenA : lexer cs
lexer ('B':cs) = TokenB : lexer cs
lexer ('C':cs) = TokenC : lexer cs


-- Main entry point
/* main = getContents >>= print . calc . lexer */

}
