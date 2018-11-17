-- Example adapted from p40 of
-- https://www2.eecs.berkeley.edu/Pubs/TechRpts/1997/CSD-97-946.pdf

{
-- Top level Haskell stuff copied to output file
module PaperExample where

import Control.Monad
import Data.List
}

%wrapper "monad"

-- -----------------------------------------------------------------------------
-- Alex "Character set macros"

-- Attach symbolic names to regular expressions.
$whitespace     = [\ \t]
-- $notstar        = [^*]
-- $notnl          = [^\n] -- (. # \n)
$notnl          = . # \n
@whitespace     = $whitespace+
@ident          = [_a-zA-Z][_a-zA-Z0-9]*
@intconst       = [1-9][0-9]*
-- @comment        = "/*"([^*]|\*[^/])*"*/"
@comment        = "/*"([^\*]|\*[^\/])*"*/"

-- -----------------------------------------------------------------------------
-- Alex "Identifier"


-- -----------------------------------------------------------------------------
-- Alex "Rules"

Example :-

-- match in all contexts
@comment            { mkToken CMNT }
@whitespace         { mkToken WS }
\n                  { mkToken WS `andBegin` pp_directive }
<pp_directive> {
 () /(@whitespace|@comment)*"#" { begin pp_directive }
 "#"                            { mkToken PND }
 ()                             { begin 0 } -- reset pp_directive
}
<pp_directive> "if" { mkToken PP_IF }
-- <pp_directive> "#if" { mkToken PP_IF }
<0> {
  "#"                 { mkToken PND }
  "("                 { mkToken LP }
  @ident              { mkToken IDENT }
  "=="                { mkToken EQEQ }
  @intconst           { mkToken INTCONST }
  ")"                 { mkToken RP }
  }
-- Collect contiguous, otherwise-unmatched text into an error token.
<0> .                   { mkToken ERROR_TOKEN }


-- -----------------------------------------------------------------------------
-- Alex "Haskell code fragment bottom"

{

-- -----------------------------------------------------------------------------

-- The token type
data Token
  = Tok
    { tokType :: TokenType
    , tokLexeme :: String
    , tokState :: Int
    -- state, lookahead and lookback are maintained implicitly
    }
instance Show Token where
  show (Tok t s st) = intercalate " " ["Tok",show t,show s,show st]

data TokenType
  = CMNT
  | WS
  | PP_IF
  | PND
  | LP
  | IDENT
  | EQEQ
  | INTCONST
  | RP
  | ERROR_TOKEN
  | EOF
  deriving Show

-- alexEOF = return [EOF]
alexEOF = return (Tok EOF "" 0)

mkToken :: TokenType -> AlexInput -> Int -> Alex Token
mkToken t = \(_,_,_,s) n -> return (Tok t (take n s) (-1))

lexShow :: String -> String
lexShow s = case lexTokenStream s of
  Left err -> err
  Right toks -> unlines $ map show toks

lexTokenStream :: String -> Either String [Token]
lexTokenStream buf
  = case unAlex go initState of
      Left str -> Left str
      Right (_,toks) -> Right toks
  where
    initState :: AlexState
    initState = (AlexState {alex_pos = alexStartPos,
                        alex_inp = buf,
                        alex_chr = '\n',
                        alex_bytes = [],
                        alex_scd = pp_directive})
    go :: Alex [Token]
    go = do
      ltok <- alexMonadScan
      sc <- alexGetStartCode
      case ltok of
        (Tok EOF _ _) -> return []
        _ -> liftM (ltok { tokState = sc } :) go

main = do
  print . runAlex "/* baz */" $ alexMonadScan
  print . runAlex "\t" $ alexMonadScan
  print . runAlex " " $ alexMonadScan
  print . runAlex "#if" $ alexMonadScan
  print . runAlex "/* comment */#if" $ alexMonadScan
  print . runAlex " /* comment */#if" $ alexMonadScan
  print . runAlex "#" $ alexMonadScan
  print . runAlex "(" $ alexMonadScan
  print . runAlex "foo" $ alexMonadScan
  print . runAlex "==" $ alexMonadScan
  print . runAlex "123" $ alexMonadScan
  print . runAlex ")" $ alexMonadScan

}
