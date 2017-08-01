{
{-# LANGUAGE OverloadedStrings #-}
module Repetitive where

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
      'a'             { TokenA }
      'b'             { TokenB }
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

Bs : B Bs          { $1:$2 }
  | {- nothing -}  { [] }

B : 'b'            { B }

C : 'c'         { () }
  | {- nothing -} { () }
{
parseError :: [t] -> a
parseError _ = error "Parse error"

data Root = Root [B]
      deriving Show

data B = B
     deriving Show

data Token
      = TokenA
      | TokenB
      | TokenC
 deriving Show



lexer :: String -> [HappyInput]
lexer str = [mkTokensNode (lexer' str)]

lexer' [] = []
lexer' (c:cs)
      | isSpace c = lexer' cs
lexer' ('a':cs) = mkTok TokenA : lexer' cs
lexer' ('b':cs) = mkTok TokenB : lexer' cs
lexer' ('c':cs) = mkTok TokenC : lexer' cs
lexer' (unk:cs) = error $ "lexer' failure on char " ++ show unk


-- Main entry point. "calc" is the parser entry point generated above
/* main = getContents >>= print . calc . lexer */

}
