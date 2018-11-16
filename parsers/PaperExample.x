-- Example adapted from p40 of
-- https://www2.eecs.berkeley.edu/Pubs/TechRpts/1997/CSD-97-946.pdf

{
-- Top level Haskell stuff copied to output file
module PaperExample where

}

%wrapper "monad"

-- -----------------------------------------------------------------------------
-- Alex "Character set macros"

-- Attach symbolic names to regular expressions.
$whitespace     = [ \t]
-- $notstar        = [^*]

@whitespace     = $whitespace*
@ident          = [_a-zA-Z][_a-zA-Z0-9]*
@intconst       = [1-9][0-9]*
-- @comment        = "/*"([^*]|\*[^/])*"*/"
@comment        = "/*"([^\*]|\*[^\/])*"*/"

-- -----------------------------------------------------------------------------
-- Alex "Identifier"


-- -----------------------------------------------------------------------------
-- Alex "Rules"

Example :-

@comment    { mkToken CMNT }
@whitespace { mkToken WS }
\n { begin 0 }
^(@whitespace|@comment)*\# { begin pp_directive }
<pp_directive> "if" { mkToken PP_IF }
"#"                 { mkToken PND }
"("                 { mkToken LP }
@ident              { mkToken IDENT }
"=="                { mkToken EQEQ }
@intconst           { mkToken INTCONST }
")"                 { mkToken RP }
-- -- Collect contiguous, otherwise-unmatched text into an error token.
.                   { mkToken ERROR_TOKEN }


-- -----------------------------------------------------------------------------
-- Alex "Haskell code fragment bottom"

{

{- original example
%start pp_directive
%%
-- Patterns:                     Rules:
{comment}                     return CMNT();
{whitespace}                  return WS();
\n                            BEGIN(INITIAL); return WS();
ˆ/({whitespace}|{comment})*#  BEGIN(pp_directive);
<pp_directive>if              return PP_IF();
"#"                           return PND();
"("                           return LP();
{ident}                       return IDENT();
"=="                          return EQEQ();
{intconst}                    return INTCONST();
")"                           return RP();
Collect contiguous, otherwise-unmatched text into an error token.
.                             error();
%%
-}
-- -----------------------------------------------------------------------------

-- push :: Token -> AlexAction [Token]
-- push x = \_ _ -> do
--   xs <- alexMonadScan
--   pure (x:xs)

-- The token type

data Token
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
alexEOF = return EOF

mkToken t = \_ _ -> return t

main = do
  print . runAlex "foo bar" $ alexMonadScan

-- TODO: look at push function in
-- https://github.com/aiya000/learning-Haskell/blob/3c9307776d0421a1aa3f1bea59adfd2593f519bc/Room/Alex/monad_wrapper.x

}
