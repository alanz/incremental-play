-- Example adapted from p40 of
-- https://www2.eecs.berkeley.edu/Pubs/TechRpts/1997/CSD-97-946.pdf

{
-- Top level Haskell stuff copied to output file
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP,MagicHash #-}
module PaperExample where

import Control.Monad
import Data.List
-- -----------------------------------------------------------------------------
-- Derived from the "monad" wrapper

import Control.Applicative as App (Applicative (..))
import qualified Control.Monad (ap)
import Data.Word (Word8)
import Data.Char (ord)
import qualified Data.Bits

#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#elif defined(__GLASGOW_HASKELL__)
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Array.Base (unsafeAt)
#else
import Array
#endif
#if __GLASGOW_HASKELL__ >= 503
import System.IO
import System.IO.Unsafe
import Debug.Trace
#else
import IO
import IOExts
#endif
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif

}

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

-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.



-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]



type Byte = Word8

-- -----------------------------------------------------------------------------
-- The input type


type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,_ps,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_p,c,_bs,_s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = trace ("alexGetByte1:b=" ++ show b) $ Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c
                                  (b:bs) = utf8Encode c
                              in( trace ("alexGetByte2:b=" ++ show b) p') `seq`  Just (b, (p', c, bs, s))

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.


data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+alex_tab_size-1) `div` alex_tab_size)*alex_tab_size+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)


-- -----------------------------------------------------------------------------
-- Default monad


data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],
        alex_scd :: !Int        -- the current startcode
    }

-- Compile with -funbox-strict-fields for best results!

runAlex :: String -> Alex a -> Either String a
runAlex input__ (Alex f)
   = case f (AlexState {alex_pos = alexStartPos,
                        alex_inp = input__,
                        alex_chr = '\n',
                        alex_bytes = [],



                        alex_scd = 0}) of Left msg -> Left msg
                                          Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Functor Alex where
  fmap f a = Alex $ \s -> case unAlex a s of
                            Left msg -> Left msg
                            Right (s', a') -> Right (s', f a')

instance Applicative Alex where
  pure a   = Alex $ \s -> Right (s, a)
  fa <*> a = Alex $ \s -> case unAlex fa s of
                            Left msg -> Left msg
                            Right (s', f) -> case unAlex a s' of
                                               Left msg -> Left msg
                                               Right (s'', b) -> Right (s'', f b)

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of
                                Left msg -> Left msg
                                Right (s',a) -> unAlex (k a) s'
  return = App.pure

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} ->
        Right (s, (pos,c,bs,inp__))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,bs,inp__)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} of
                  state__@(AlexState{}) -> Right (state__, ())

alexError :: String -> Alex a
alexError message = Alex $ const $ Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())


alexMonadScan = do
  inp__ <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
    AlexSkip  inp__' _len -> do
        alexSetInput inp__'
        alexMonadScan
    AlexToken inp__' len action -> do
        alexSetInput inp__'
        action (ignorePendingBytes inp__) len

-- -----------------------------------------------------------------------------
-- Useful token actions

type AlexAction result = AlexInput -> Int -> Alex result

-- just ignore this token and scan another one
-- skip :: AlexAction result
skip _input _len = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code _input _len = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input__ len = do
  alexSetStartCode code
  action input__ len

token :: (AlexInput -> Int -> token) -> AlexAction token
token t input__ len = return (t input__ len)

-- End of copy over from monad wrapper
------------------------------------------------------------------------

}
