{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
module Repetitive3 where

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
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import qualified System.IO as Happy_System_IO
import qualified System.IO.Unsafe as Happy_System_IO_Unsafe
import qualified Debug.Trace as Happy_Debug_Trace
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

-- using template file ./happy-templates/IncrementalTemplate-ghc-debug

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13
	= HappyTerminal (TokenL TokenType)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13

	deriving Show


data Tok = Tok Happy_GHC_Exts.Int# (TokenL TokenType)
  deriving Show
instance Pretty Tok
happyExpList :: HappyAddr
happyExpList = HappyA# "\x18\x00\x00\x01\x00\xc0\x21\x00\x00\x00\x20\x00\x00\x00\x00\x00\xd6\x03\x00\x00\x00\x08\x04\x00\x00\x00\xd4\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_calc","Ultraroot","bos","eos","tree","Root","A","Bs","B","C","listb__B__","'a'","'b'","'B'","'d'","'D'","'c'","%eof"]
        bit_start = st * 20
        bit_end = (st + 1) * 20
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..19]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyGotoValidArray :: HappyAddr
happyGotoValidArray = HappyA# "\x30\x00\x08\x00\x38\x00\x00\x40\x00\x00\x00\xc0\x02\x00\x00\x10\x00\x00\x80\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyGotoValid #-}
happyGotoValid st nt = valid
  where bit_nr = nt + st * 14
        valid = readArrayBit happyGotoValidArray bit_nr

happyFragileStateArray :: HappyAddr
happyFragileStateArray = HappyA# "\x00\x00\x01\x00\x00\x00"#

{-# NOINLINE happyFragileState #-}
happyFragileState st = fragile
  where bit_nr = st
        fragile = readArrayBit happyFragileStateArray bit_nr

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x21\x00\x23\x00\x0e\x00\x13\x00\x18\x00\x00\x00\xfa\xff\x00\x00\x0c\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x20\x00\x15\x00\x1a\x00\x00\x00\x0b\x00\x00\x00\x11\x00\x00\x00\x03\x00\x00\x00\xfc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xfd\xff\x00\x00\xf8\xff\x00\x00\xfc\xff\xfb\xff\x00\x00\xf9\xff\xf1\xff\xf0\xff\xf7\xff\xf6\xff\xf5\xff\xf4\xff\xf3\xff\xfe\xff\xef\xff\xfa\xff\xf2\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x07\x00\x08\x00\x07\x00\x0a\x00\x09\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x08\x00\x08\x00\x0a\x00\x02\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x04\x00\x05\x00\x06\x00\x09\x00\x01\x00\x06\x00\x07\x00\x0b\x00\x09\x00\x03\x00\x10\x00\x03\x00\x04\x00\x05\x00\x00\x00\x01\x00\x01\x00\x02\x00\x11\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x09\x00\x0a\x00\x09\x00\x0b\x00\x10\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x0a\x00\x11\x00\x11\x00\x0f\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x05\x00\x06\x00\x07\x00\x12\x00\x02\x00\x08\x00\x09\x00\x08\x00\x0a\x00\x10\x00\x13\x00\x04\x00\x05\x00\x06\x00\x03\x00\x02\x00\x04\x00\x03\x00\xff\xff\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#


-- [(ActionEntry,0,-3,1,2,[(1,4),(2,3)]),(ActionEntry,1,0,0,1,[(2,3)]),(ActionEntry,2,-8,7,4,[(4,5),(5,6),(6,7),(11,8)]),(ActionEntry,3,0,0,1,[(17,-1)]),(ActionEntry,4,-4,0,1,[(3,16)]),(ActionEntry,5,-5,0,0,[]),(ActionEntry,6,0,8,7,[(7,9),(8,10),(10,11),(12,12),(13,13),(14,14),(15,15)]),(ActionEntry,7,-7,0,0,[]),(ActionEntry,8,-15,7,2,[(9,18),(16,19)]),(ActionEntry,9,-16,0,0,[]),(ActionEntry,10,-9,7,6,[(8,10),(10,17),(12,12),(13,13),(14,14),(15,15)]),(ActionEntry,11,-10,0,0,[]),(ActionEntry,12,-11,0,0,[]),(ActionEntry,13,-12,0,0,[]),(ActionEntry,14,-13,0,0,[]),(ActionEntry,15,-2,0,0,[]),(ActionEntry,16,-17,7,6,[(8,10),(10,17),(12,12),(13,13),(14,14),(15,15)]),(ActionEntry,17,-6,0,0,[]),(ActionEntry,18,-14,0,0,[])]

happyReduceArr = Happy_Data_Array.array (1, 16) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16)
	]

happy_n_terms = 8 :: Int
happy_n_nonterms = 10 :: Int

happyReduce_1 am fragile = happySpecReduce_3  am 0# (happyReduction_1 fragile)
happyReduction_1 fragile p3
	p2@(Node (Val {here = (HappyAbsSyn7  happy_var_2)}) _)
	p1
	 =  mkNode (HappyAbsSyn4
		 (happy_var_2
	)) (Just 1) fragile [p1,p2,p3]
happyReduction_1 fragile _ _ _  = notHappyAtAll 

happyReduce_2 am fragile = happySpecReduce_0  am 1# (happyReduction_2 fragile)
happyReduction_2 fragile  =  mkNode (HappyAbsSyn5
		 (()
	)) (Just 2) fragile []

happyReduce_3 am fragile = happySpecReduce_0  am 2# (happyReduction_3 fragile)
happyReduction_3 fragile  =  mkNode (HappyAbsSyn6
		 (()
	)) (Just 3) fragile []

happyReduce_4 am fragile = happySpecReduce_1  am 3# (happyReduction_4 fragile)
happyReduction_4 fragile p1@(Node (Val {here = (HappyAbsSyn8  happy_var_1)}) _)
	 =  mkNode (HappyAbsSyn7
		 (happy_var_1
	)) (Just 4) fragile [p1]
happyReduction_4 fragile _  = notHappyAtAll 

happyReduce_5 am fragile = happySpecReduce_3  am 4# (happyReduction_5 fragile)
happyReduction_5 fragile p3
	p2@(Node (Val {here = (HappyAbsSyn10  happy_var_2)}) _)
	p1
	 =  mkNode (HappyAbsSyn8
		 (Root happy_var_2
	)) (Just 5) fragile [p1,p2,p3]
happyReduction_5 fragile _ _ _  = notHappyAtAll 

happyReduce_6 am fragile = happySpecReduce_1  am 5# (happyReduction_6 fragile)
happyReduction_6 fragile p1
	 =  mkNode (HappyAbsSyn9
		 (()
	)) (Just 6) fragile [p1]

happyReduce_7 am fragile = happySpecReduce_0  am 5# (happyReduction_7 fragile)
happyReduction_7 fragile  =  mkNode (HappyAbsSyn9
		 (()
	)) (Just 6) fragile []

happyReduce_8 am fragile = happySpecReduce_1  am 6# (happyReduction_8 fragile)
happyReduction_8 fragile p1@(Node (Val {here = (HappyAbsSyn13  happy_var_1)}) _)
	 =  mkNode (HappyAbsSyn10
		 (toList happy_var_1
	)) (Just 7) fragile [p1]
happyReduction_8 fragile _  = notHappyAtAll 

happyReduce_9 am fragile = happySpecReduce_1  am 7# (happyReduction_9 fragile)
happyReduction_9 fragile p1
	 =  mkNode (HappyAbsSyn11
		 (BL
	)) (Just 8) fragile [p1]

happyReduce_10 am fragile = happySpecReduce_1  am 7# (happyReduction_10 fragile)
happyReduction_10 fragile p1
	 =  mkNode (HappyAbsSyn11
		 (BU
	)) (Just 8) fragile [p1]

happyReduce_11 am fragile = happySpecReduce_1  am 7# (happyReduction_11 fragile)
happyReduction_11 fragile p1
	 =  mkNode (HappyAbsSyn11
		 (Bd
	)) (Just 8) fragile [p1]

happyReduce_12 am fragile = happySpecReduce_1  am 7# (happyReduction_12 fragile)
happyReduction_12 fragile p1
	 =  mkNode (HappyAbsSyn11
		 (BD
	)) (Just 8) fragile [p1]

happyReduce_13 am fragile = happySpecReduce_1  am 8# (happyReduction_13 fragile)
happyReduction_13 fragile p1
	 =  mkNode (HappyAbsSyn12
		 (()
	)) (Just 9) fragile [p1]

happyReduce_14 am fragile = happySpecReduce_0  am 8# (happyReduction_14 fragile)
happyReduction_14 fragile  =  mkNode (HappyAbsSyn12
		 (()
	)) (Just 9) fragile []

happyReduce_15 am fragile = happySpecReduce_1  am 9# (happyReduction_15 fragile)
happyReduction_15 fragile p1@(Node (Val {here = (HappyAbsSyn11  happy_var_1)}) _)
	 =  mkNode (HappyAbsSyn13
		 (BSingle happy_var_1
	)) (Just 10) fragile [p1]
happyReduction_15 fragile _  = notHappyAtAll 

happyReduce_16 am fragile = happySpecReduce_2  am 9# (happyReduction_16 fragile)
happyReduction_16 fragile p2@(Node (Val {here = (HappyAbsSyn13  happy_var_2)}) _)
	p1@(Node (Val {here = (HappyAbsSyn13  happy_var_1)}) _)
	 =  mkNode (HappyAbsSyn13
		 (BDouble happy_var_1 happy_var_2
	)) (Just 10) fragile [p1,p2]
happyReduction_16 fragile _ _  = notHappyAtAll 

happyNewToken verifying action sts stk [] =
	happyDoAction NotVerifying 17# (mkTokensNode [Tok 17# notHappyAtAll]) action sts stk []

happyNewToken verifying action sts stk (t:ts) =
	let cont i inp ts' = happyDoAction verifying i inp action sts stk ts' in
	case getTerminals t of {
	  [] -> cont 0# t ts;
	  (Tok _ tk:tks) ->
	    case tk of {
		TokL { tokType = T TokenA  } -> cont 11# (setTerminals t (Tok 11# tk:tks)) ((setTerminals t tks):ts);
		TokL { tokType = T TokenBL } -> cont 12# (setTerminals t (Tok 12# tk:tks)) ((setTerminals t tks):ts);
		TokL { tokType = T TokenBU } -> cont 13# (setTerminals t (Tok 13# tk:tks)) ((setTerminals t tks):ts);
		TokL { tokType = T TokenBd } -> cont 14# (setTerminals t (Tok 14# tk:tks)) ((setTerminals t tks):ts);
		TokL { tokType = T TokenBD } -> cont 15# (setTerminals t (Tok 15# tk:tks)) ((setTerminals t tks):ts);
		TokL { tokType = T TokenC  } -> cont 16# (setTerminals t (Tok 16# tk:tks)) ((setTerminals t tks):ts);
		_ -> happyError' ((t:ts), [])
		};

	};

happyError_ explist 17# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(t)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
calc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> case x of {Node (Val { here = HappyAbsSyn4 z }) _ -> happyReturn x; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [t] -> a
parseError _ = error "Parse error"

data BinaryT a
  = BEmpty
  | BSingle a
  | BDouble (BinaryT a) (BinaryT a)
  deriving (Show, Foldable)

data Root = Root [B]
      deriving Show

data B = BL | BU | Bd | BD
     deriving Show

/* data Token */
/*       = TokenA */
/*       | TokenBL */
/*       | TokenBU */
/*       | TokenBd */
/*       | TokenBD */
/*       | TokenC */
/*  deriving Show */

mylexer :: String
             -> [Node (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13) Tok]
mylexer s = [mkTokensNode toks]
  where
    toks = lll s

lll :: String -> [Tok]
lll s =
  case runAlex s (lexer cc) of
    Left err -> error err
    Right v  -> v
  where
    cc :: LT.TokenL TokenType -> Alex [Tok]
    cc ltok = case LT.tokType ltok of
      LT.T EOF -> return []
      LT.T WS  -> lexer cc
      _        -> (mkTok ltok :) <$> lexer cc


-- lexer :: String -> [HappyInput]
lexerOrig str = [mkTokensNode (lexer' str)]

lexer' :: String -> [Tok]
lexer' [] = []
lexer' (c:cs)
      | isSpace c = lexer' cs
lexer' ('a':cs) = mt "a" TokenA  : lexer' cs
lexer' ('b':cs) = mt "b" TokenBL : lexer' cs
lexer' ('B':cs) = mt "B" TokenBU : lexer' cs
lexer' ('d':cs) = mt "d" TokenBd : lexer' cs
lexer' ('D':cs) = mt "D" TokenBD : lexer' cs
lexer' ('c':cs) = mt "c" TokenC  : lexer' cs
lexer' (unk:cs) = error $ "lexer' failure on char " ++ show unk

mt s t = mkTok (LT.mkTok s t)

-- Main entry point. "calc" is the parser entry point generated above
/* main = getContents >>= print . calc . lexer */
{-# LINE 1 "happy-templates/GenericTemplate.hs" #-}
{-# LINE 1 "happy-templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "/usr/include/stdc-predef.h" #-}

{-# LINE 17 "/usr/include/stdc-predef.h" #-}














































{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "/opt/ghc/8.6.3/lib/ghc-8.6.3/include/ghcversion.h" #-}















{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc11937_0/ghc_2.h" #-}
































































































































































































{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "happy-templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

{-# LINE 13 "happy-templates/GenericTemplate.hs" #-}





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 46 "happy-templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList







{-# LINE 77 "happy-templates/GenericTemplate.hs" #-}

{-# LINE 91 "happy-templates/GenericTemplate.hs" #-}



happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO $ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    return expr






mkTok t = Tok -10# t
-- nullTok = Tok -10# notHappyAtAll
-- nullTok = Tok -10# TokenPlus
-- nullTok = Tok -10# TokenA







infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-- AZ: following to come out of happy ProduceCode
-- type HappyAbsSynType = HappyAbsSyn Exp () () Exp Exp Term Factor
-- type HappyAbsSynType = HappyAbsSyn () () () Exp Exp
-- type HappyAbsSynType = HappyAbsSyn Root () () Root Root () [B] B ()
-- type HappyAbsSynType = HappyAbsSyn Root () () Root Root () [B] B () (BinaryT B)
{-
7 = 4
5 = ()
6 = ()
8 = 7 = Root
10 = [B]
9 = ()
13 = BTree B
10 = [B]
11 = B
12 = ()
13 = BTree B
-}


-- type NodeVal = Val HappyAbsSynType Tok

-- instance Pretty HappyAbsSynType

data DoACtionMode = Normal | AllReductions
                  deriving Eq

data Verifying = Verifying | NotVerifying
                  deriving (Eq, Show)

-----------------------------------------------------------------------------
-- starting the parse


happyNodeSentinel = mkNode (HappyErrorToken (-1000)) Nothing False []

-- happyParse :: Happy_GHC_Exts.Int# -> [HappyInput] -> HappyIdentity HappyInput
happyParse start_state = happyNewToken NotVerifying start_state (HappyCons ((-1000#)) (notHappyAtAll)) (happyNodeSentinel `HappyStk` notHappyAtAll)

-- showStacks :: Happy_IntList -> HappyStk HappyInput -> String
showStacks ((HappyCons ((-1000#)) (_))) _ = "[]"
showStacks ((HappyCons (st) (sts))) ((Node v _) `HappyStk` stks)
  = show ((Happy_GHC_Exts.I# (st)),take 40 $ showHere v) ++ ":" ++ showStacks sts stks

-- showInputQ :: [HappyInput] -> String
showInputQ is = "[" ++ intercalate "," (map (showHere . rootLabel) is) ++ "]"

-- showInput :: [HappyInput] -> String
showInput ts = "[" ++ intercalate "," (map (showHere . rootLabel) ts) ++ "]"

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays in incremental mode: do the next action



-- | A Node stores the components of the parse tree as it is built. It is
-- versioned, and provides the basis for the re-use of prior parse information
-- when incremental changes occur.
-- It is parameterised by the HappyAbsSynType
type Node a b = Tree (Val a b)
-- TODO:consider using a DualTree instead, with monoidal instances for the
-- change flag and next/last terminal propagation

-- instance (Show a, Show b) => Show (Node a b) where
--   show (Node (Val cl cc h ts nt) cs) = intercalate " " ["Node",show cl, show cc,"(" ++ show h ++ ")",show cs,show ts, show nt]
instance (Show a, Pretty a, Show b, Pretty b) => Pretty (Node a b) where
  pretty (Node (Val cl cc h hnt ts nt lt lf rf gf) cs)
    = "Node" <+> pretty cl <+> pretty cc <+> parens (pretty nt) <+> parens (pretty lt)
             <+> parens (pretty hnt)
             <+> pretty lf <+> pretty rf <+> pretty gf
           <> line <> (indent 3 (pretty h))
           <> line <> (indent 4 (pretty cs))
           <> line <> (indent 4 (pretty ts))

-- TODO: consider space-efficient storage of the Val structure. bitfields, what else?
data Val a b = Val
  { changedLocal   :: !Bool
  , changedChild   :: !Bool -- ^set if any of the children have a change
  , here           :: !a
  , here_nt        :: !(Maybe Int)
  , terminals      :: ![b]
  , next_terminal  :: !(Maybe b)  -- ^ the leftmost terminal of the yield of the tree
  , last_terminal  :: !(Maybe b)  -- ^ the rightmost terminal of the yield of the tree
  , leftFragile    :: !Bool       -- ^ Fragile on leftmost edge
  , rightFragile   :: !Bool       -- ^ Fragile on rightmost edge
  , grammarFragile :: !Bool       -- ^ The grammar production used to produce
                                  -- this node is fragile (Has a conflict, or
                                  -- precedence)
  }
instance (Show a, Show b) => Show (Val a b) where
  show v@(Val cl cc h hnt ts nt lt lf rf gf)
    = unwords ["Val",showChanged v
              , showFragile v
              , "(" ++ show h ++ ")"
              , "(" ++ show hnt ++ ")",show ts
              , "(" ++ show nt ++ ")", "(" ++ show lt ++ ")"
              ]
instance (Show a, Pretty a, Show b, Pretty b) => Pretty (Val a b) where
  pretty ((Val cl cc h hnt ts nt lt lf rf gf) )
    = "Val" <+> pretty cl <+> pretty cc <+> parens (pretty nt) <+> parens (pretty lt) <+> parens (pretty hnt)
           <+> pretty lf <+> pretty rf <+> pretty gf
           <> line <> (indent 3 (pretty h))
           <> line <> (indent 4 (pretty ts))

showHere :: (Show a, Show b) => Val a b -> String
showHere v@(Val { here = h, here_nt = Nothing, terminals = ts  })
  = showFragile v ++ "T "                   ++ show h -- ++ " " ++ show ts
showHere v@(Val { here = h, here_nt = Just nt, terminals = ts  })
  = showFragile v ++ "NT" ++ show nt ++ " " ++ show h -- ++ " " ++ show ts

showChanged :: (Show a, Show b) => Val a b -> String
showChanged Val { changedLocal = l, changedChild = c }
  = concat ["[ch:",mt "L" l, mt "C" c, "]"]
  where
    mt str True  = str
    mt _   False = ""

showFragile :: (Show a, Show b) => Val a b -> String
showFragile Val { grammarFragile = g, leftFragile = l, rightFragile = r}
  = concat ["[fr:",mt "G" g, mt "L" l, mt "R" r, "]"]
  where
    mt str True  = str
    mt _   False = ""

mkNode x mnt gf cs
  = Node (Val
         { here = x
         , here_nt = mnt
         , changedLocal = False, changedChild = False
         , terminals = []
         , next_terminal = getNextTerminal cs
         , last_terminal = getLastTerminal cs
         , leftFragile   = goL cs
         , rightFragile  = goR cs
         , grammarFragile = gf
         }) cs
  where
    goL [] = False
    goL ((Node v _):cs')
      = if grammarFragile v || leftFragile v
          then True
          else case next_terminal v of
                 Nothing  -> goL cs'
                 Just  _  -> False
    goR [] = False
    goR ((Node v _):cs')
      = if grammarFragile v || rightFragile v
          then True
          else case last_terminal v of
                 Nothing  -> goR cs'
                 Just  _  -> False

getNextTerminal :: [Node a b] -> Maybe b
getNextTerminal [] = Nothing
getNextTerminal cs
  = case catMaybes (map (next_terminal . rootLabel) cs) of
      []     -> Nothing
      (nt:_) -> Just nt

getLastTerminal :: [Node a b] -> Maybe b
getLastTerminal [] = Nothing
getLastTerminal cs
  = case catMaybes (map (last_terminal . rootLabel) cs) of
      [] -> Nothing
      ls -> Just (last ls)

mkNodeNt x mnt gf cs nt
  = let Node v cs' = (mkNode x mnt gf cs)
    in Node (v { next_terminal = Just nt, last_terminal = Just nt, terminals = [nt] }) cs'

isFragile :: (Node a b) -> Bool
isFragile (Node v _) = grammarFragile v || leftFragile v || rightFragile v

-- AZ:NOTE: The second param below (Token) can/should be moved into the Input
-- type, as it is meaningless for a nonterminal. But what about compatibility
-- with other happy options?
--
-- The problem comes from the mapping of a Token to a unique number in
-- happyNewToken
--
-- For now, keep it outside, but give an error value when processing a NonTerminal
-- This leads to the unfortunate creation of a second input type.
-- data ParserInput a
--   = InputToken Token
--   | InputTree a

-- data Tok = Tok Happy_GHC_Exts.Int# Token
--   deriving Show
-- instance Pretty Tok

-- type HappyInput = Node HappyAbsSynType Tok

mkTokensNode tks = setTerminals (mkNode (HappyErrorToken (-5)) Nothing False []) tks

setTerminals :: Node a b -> [b] -> Node a b
setTerminals (Node v cs) ts = Node (v { terminals = ts}) cs

getTerminals :: Node a b -> [b]
getTerminals (Node v cs) = terminals v

-- happyDoAction :: Verifying
--               -> Happy_GHC_Exts.Int#   -- ^ Current lookahead token number
--               -> HappyInput -- ^ input being processed. "parse stack" from the paper  Same as first item on input list?
--               -> Happy_GHC_Exts.Int#   -- ^ Current state
--               -> Happy_IntList -> HappyStk HappyInput -- ^ Current state and shifted item stack
--               -> [HappyInput] -- ^ Input being processed
--               -> HappyIdentity HappyInput
happyDoAction verifying la inp@(Node v@(Val {terminals = toks, next_terminal = mnext, here_nt = mnt}) cs) st sts stk tks
  = (happyTrace ("--------------------------\n")) $
    (happyTrace ("happyDoAction:tks=" ++ showInputQ tks ++ "\n")) $
    (happyTrace ("happyDoAction:stacks=" ++ showStacks sts stk ++ "\n")) $
    (happyTrace ("happyDoAction:inp=" ++ showHere v ++ "\n")) $
    (happyTrace ("happyDoAction:verifying=" ++ show verifying ++ "\n")) $
    case toks of -- Terminals
      (tok@(Tok i tk):ts) ->
        (happyTrace ("t:state: " ++ show (Happy_GHC_Exts.I# (st)) ++                     ",\tfragile: " ++ show fragile ++                     ",\ttoken: " ++ show (Happy_GHC_Exts.I# (i)) ++                     ",\taction: ")) $



        case action of
              0#   -> (happyTrace ("fail.\n")) $
                           if verifying == Verifying
                             then rightBreakdown st sts stk tks
                             -- else happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i inp st sts stk tks
                             else happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) 0# inp st sts stk tks
              -1#  -> (happyTrace ("accept. A\n")) $
                             happyAccept i tk st sts stk tks
              n | LT(n,(0# :: Happy_GHC_Exts.Int#))
                        -> (happyTrace ("reduce (rule " ++ show rule ++ ")")) $
                           (happyReduceArr Happy_Data_Array.! rule) NotVerifying fragile i inp st sts stk tks
                            where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
              n         -> (happyTrace ("shift, enter state "                                        ++ show (Happy_GHC_Exts.I# (new_state))                                        ++ "\n")) $


                           happyShift NotVerifying new_state i (mkNodeNt (HappyTerminal tk) Nothing fragile [] tok) st sts stk tks
                           where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
        where action = lookupAction st i
              fragile = happyFragileState (Happy_GHC_Exts.I# (st))
      _ -> -- Non-terminal input
        (happyTrace ("nt:state: " ++ show (Happy_GHC_Exts.I# (st)) ++                     ",\tfragile: " ++ show (happyFragileState (Happy_GHC_Exts.I# (st))) ++                     ",\ttree: " ++ (take 35 $ show (here $ rootLabel inp)) ++                     ",\taction: ")) $



        if changed inp || isFragile inp
          then (happyTrace ("left breakdown.\n")) $
               leftBreakdown verifying la inp st sts stk tks
          else
            case mnt of
              Just ((Happy_GHC_Exts.I# (i))) ->
                (happyTrace ("nt:" ++ show ((Happy_GHC_Exts.I# (i))) ++ ",actionv:" ++ show ((Happy_GHC_Exts.I# (action))) ++ ":")) $
-------------------------------
                case action of
                      0#           -> (happyTrace ("fail.\n")) $
                                           if null cs
                                             then happyNewToken verifying st sts stk tks
                                             else leftBreakdown NotVerifying la inp st sts stk tks
                      -1#          -> (happyTrace ("nt:accept. A\n")) $
                                             -- This can never happen
                                             notHappyAtAll
                      n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> (happyTrace ("reduce (rule " ++ show rule                                                                      ++ ")")) $

                                                         (happyReduceArr Happy_Data_Array.! rule) NotVerifying fragile i inp st sts stk tks
                                                         where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                      n                 -> (happyTrace ("shift, enter state "                                                        ++ show (Happy_GHC_Exts.I# (new_state))                                                        ++ "\n")) $


                                           happyShift Verifying new_state i (Node v' cs) st sts stk tks
                                           where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
                                                 v' = v { grammarFragile = fragile }
                where action = lookupAction st i
                      fragile = happyFragileState (Happy_GHC_Exts.I# (st))
-------------------------------
              Nothing -> (happyTrace ("mnext == Nothing.\n")) $
                          happyNewToken NotVerifying st sts stk tks


-- leftBreakdown :: Verifying
--               -> Happy_GHC_Exts.Int#   -- ^ Current lookahead token number
--               -> HappyInput -- ^ input being processed. "parse stack" from the paper Same as first item on input list?
--               -> Happy_GHC_Exts.Int#   -- ^ Current state
--               -> Happy_IntList -> HappyStk HappyInput -- ^ Current state and shifted item stack
--               -> [HappyInput] -- ^ Input being processed
--               -> HappyIdentity HappyInput
leftBreakdown verifying la inp@(Node v cs) st sts stk ts
  = (happyTrace ("leftBreakdown:ts=" ++ showInputQ ts ++ "\n")) $
    (happyTrace ("leftBreakdown:inp=" ++ showHere v ++ "\n")) $
    case cs of
      []    -> (happyTrace ("leftBreakdown:no children\n")) $
               -- happyNewToken verifying st sts stk ts
               -- happyNewToken verifying st sts stk (inp':ts)
               happyDoAction verifying la inp' st sts stk ts
               where inp' = Node (v { changedLocal = False, changedChild = False}) cs
      (c:cs') -> if isFragile c
                   then (happyTrace ("leftBreakdown:fragile:" ++ showHere (rootLabel c) ++ "\n")) $
                        leftBreakdown verifying la c st sts stk (cs' ++ ts)
                   else (happyTrace ("leftBreakdown:not fragile\n")) $
                        happyNewToken verifying      st sts stk (cs  ++ ts)

-- rightBreakdown :: Happy_GHC_Exts.Int#   -- ^ Current state
--                -> Happy_IntList -> HappyStk HappyInput -- ^ Current state and shifted item stack
--                -> [HappyInput] -- ^ Input being processed
--                -> HappyIdentity HappyInput
rightBreakdown st sts@((HappyCons (sts1) (stss))) stk@(stk1@(Node v cs) `HappyStk` stks)
  -- Break down the right hand edge of the top of the parse stack until it is
  -- the last_terminal value of the original.
  -- Nodes not having a last_terminal are discarded (no yield)
  = (happyTrace ("rightBreakdown:stacks=" ++ (showStacks sts stk) ++ "\n")) $
    if hasYield stk1
     then case cs of
            [] -> (happyTrace ("rightBreakdown:has yield, no children, ie token:(st,sts1,stk1)=" ++                               (unwords [show (Happy_GHC_Exts.I# (st)),show (Happy_GHC_Exts.I# (sts1)), take 30 ( show (here v))]) ++ ".\n")) $

                  case last_terminal v of
                    Just (Tok i _) ->
                      case (nextStateShift sts1 i) of
                        Just ((Happy_GHC_Exts.I# (st2))) -> (happyTrace ("rightBreakdown:nextStateShift:" ++ show ((Happy_GHC_Exts.I# (sts1)),(Happy_GHC_Exts.I# (i)),(Happy_GHC_Exts.I# (st2))) ++ "\n")) $
                                            happyNewToken NotVerifying st2 sts stk
                        Nothing          -> notHappyAtAll
                    Nothing       -> (happyTrace ("rightBreakdown:no nt\n")) $
                                     happyNewToken NotVerifying sts1 sts stk
            _  -> -- shift each child onto the stack, then call rightBreakdown again
              (happyTrace ("rightBreakdown:going through children (n=" ++ show (length cs) ++ ").\n")) $
              rightBreakdown st2 sts' stk'
              where
                (st',sts',stk') = foldl' go ((Happy_GHC_Exts.I# (sts1)),stss,stks) cs
                !((Happy_GHC_Exts.I# (st2))) = st'
                -- go :: (Int, Happy_IntList, HappyStk HappyInput) -> HappyInput -> (Int, Happy_IntList, HappyStk HappyInput)
                go  ((Happy_GHC_Exts.I# (st)), sts, stk) c@(Node v@(Val {last_terminal = mtok,here_nt = mnt}) _)
                  = (happyTrace ("rightBreakdown:go:(st,v)=" ++ show ((Happy_GHC_Exts.I# (st)),take 30 $ showHere v) ++ "\n")) $
                    case (mnt, mtok) of
                      (Just ((Happy_GHC_Exts.I# (nt))), Just (Tok i tk)) ->
                        (happyTrace ("go:nt " ++ (showStacks sts stk) ++ "\n")) $
                        ((Happy_GHC_Exts.I# (nextState st nt)), (HappyCons (st) (sts)), (c `HappyStk` stk))
                      (Nothing,         Just (Tok i tk)) ->
                        (happyTrace ("go:terminal " ++ (showStacks sts stk) ++ "\n")) $
                        ((Happy_GHC_Exts.I# (nextState st i)), (HappyCons (st) (sts)), (c `HappyStk` stk))
                      _ -> (happyTrace ("rightBreakdown:no non-terminal and/or no last_terminal.\n")) $ notHappyAtAll

     else (happyTrace ("rightBreakdown,no yield, popping stack")) $ rightBreakdown sts1 stss stks

lookupAction' :: Int -> Int -> Int
lookupAction' st' i' =
  case (st',i') of
   ((Happy_GHC_Exts.I# (st)), (Happy_GHC_Exts.I# (i))) -> ((Happy_GHC_Exts.I# (lookupAction st i)))

lookupAction :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
lookupAction st i = action
        where off    = indexShortOffAddr happyActOffsets st
              off_i  = (off Happy_GHC_Exts.+# i)
              check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                       then EQ(indexShortOffAddr happyCheck off_i, i)
                       else False
              action
               | check     = indexShortOffAddr happyTable off_i
               | otherwise = indexShortOffAddr happyDefActions st


nextState' :: Int -> Int -> Int
nextState' st' nt' =
  case (st',nt') of
   ((Happy_GHC_Exts.I# (st)), (Happy_GHC_Exts.I# (nt))) -> ((Happy_GHC_Exts.I# (nextState st nt)))

nextState :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
nextState st nt =
  if happyGotoValid (Happy_GHC_Exts.I# (st)) (Happy_GHC_Exts.I# (nt))
    then new_state
    else 0#
  where off = indexShortOffAddr happyGotoOffsets st
        off_i = (off Happy_GHC_Exts.+# nt)
        new_state = indexShortOffAddr happyTable off_i

nextStateShift' :: Int -> Int -> Maybe Int
nextStateShift' st' i' =
  case (st',i') of
    (((Happy_GHC_Exts.I# (st))), ((Happy_GHC_Exts.I# (i)))) -> nextStateShift st i

nextStateShift :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int# -> Maybe Int
nextStateShift st i =
  if (GTE(action, (1# :: Happy_GHC_Exts.Int#)))
    then Just (Happy_GHC_Exts.I# ((action Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))))
    else Nothing
    -- else Just (Happy_GHC_Exts.I# (action))
  where off    = indexShortOffAddr happyActOffsets st
        off_i  = (off Happy_GHC_Exts.+# i)
        check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                 then EQ(indexShortOffAddr happyCheck off_i, i)
                 else False
        action :: Happy_GHC_Exts.Int#
        action
         | check     = indexShortOffAddr happyTable off_i
         | otherwise = indexShortOffAddr happyDefActions st

-- changed :: HappyInput -> Bool
changed (Node (Val { changedLocal = cl, changedChild = cc}) _) = cl || cc

-- hasYield :: HappyInput -> Bool
hasYield (Node (Val { last_terminal = mlt}) _) = isJust mlt



-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 560 "happy-templates/GenericTemplate.hs" #-}


indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 600 "happy-templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

-- happyShift :: Verifying
--            -> Happy_GHC_Exts.Int#  -- new state
--            -> Happy_GHC_Exts.Int#  --  Current lookahead token number
--            -> HappyInput -- current input / "parse tree"
--            -> Happy_GHC_Exts.Int#   -- current state
--            -> Happy_IntList
--            -> HappyStk HappyInput
--            -> [HappyInput]
--            -> HappyIdentity HappyInput
happyShift verifying new_state ((0#)) inp st sts stk@(x `HappyStk` _) =
     let i = (case x of { Node (Val{ here = HappyErrorToken (Happy_GHC_Exts.I# (i))}) _ -> i} ) in
--     trace "shifting the error token" $
     happyDoAction verifying i inp new_state (HappyCons (st) (sts)) (stk)

happyShift verifying new_state i inp st sts stk =
     -- (happyTrace ("happyShift:(new_state,i,inp)=" ++ show ((Happy_GHC_Exts.I# (new_state)),(Happy_GHC_Exts.I# (i)),inp) ++ "\n")) $
     happyNewToken verifying new_state (HappyCons (st) (sts)) (inp `HappyStk`stk)

-- happyReduce is specialised for the common cases.

-- happySpecReduce_0 :: Verifying
--                   -> Happy_GHC_Exts.Int#   -- Non terminal to end up on TOS
--                   -> HappyInput -- function from TOS items to new TOS
--                   -> Happy_GHC_Exts.Int#   -- input token value
--                   -> HappyInput
--                   -> Happy_GHC_Exts.Int#
--                   -> Happy_IntList
--                   -> HappyStk HappyInput
--                   -> [HappyInput]
--                   -> HappyIdentity HappyInput
happySpecReduce_0 am nt fn 0# inp st sts stk
     = happyFail [] 0# inp st sts stk
happySpecReduce_0 am nt fn j inp st@((action)) sts stk
     = happyGoto am nt j inp st (HappyCons (st) (sts)) (fn `HappyStk` stk)

-- happySpecReduce_1 :: Verifying
--                   -> Happy_GHC_Exts.Int#
--                   -> (HappyInput -> HappyInput)
--                   -> Happy_GHC_Exts.Int#
--                   -> HappyInput
--                   -> Happy_GHC_Exts.Int#
--                   -> Happy_IntList
--                   -> HappyStk HappyInput
--                   -> [HappyInput]
--                   -> HappyIdentity HappyInput
happySpecReduce_1 am i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 am nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let !r = fn v1 in -- TODO:AZ strictness?
       happySeq r (happyGoto am nt j tk st sts (r `HappyStk` stk'))

-- happySpecReduce_2 :: Verifying
--                   -> Happy_GHC_Exts.Int#
--                   -> (HappyInput -> HappyInput -> HappyInput)
--                   -> Happy_GHC_Exts.Int#
--                   -> HappyInput
--                   -> Happy_GHC_Exts.Int#
--                   -> Happy_IntList
--                   -> HappyStk HappyInput
--                   -> [HappyInput]
--                   -> HappyIdentity HappyInput
happySpecReduce_2 am i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 am nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let !r = fn v1 v2 in -- TODO:AZ strictness?
       happySeq r (happyGoto am nt j tk st sts (r `HappyStk` stk'))

-- happySpecReduce_3 :: Verifying
--                   -> Happy_GHC_Exts.Int#
--                   -> (HappyInput -> HappyInput -> HappyInput -> HappyInput)
--                   -> Happy_GHC_Exts.Int#
--                   -> HappyInput
--                   -> Happy_GHC_Exts.Int#
--                   -> Happy_IntList
--                   -> HappyStk HappyInput
--                   -> [HappyInput]
--                   -> HappyIdentity HappyInput
happySpecReduce_3 am i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 am nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let !r = fn v1 v2 v3 in -- TODO:AZ strictness?
       happySeq r (happyGoto am nt j tk st sts (r `HappyStk` stk'))

happyReduce k am i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k am nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let !r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto am nt j tk st1 sts1 r)

-- happyMonadReduce :: Happy_GHC_Exts.Int#      -- number of items to remove from stack
--                  -> Verifying
--                  -> Happy_GHC_Exts.Int#
--                  -> (Happy_IntList -> HappyStk HappyInput -> HappyIdentity HappyInput)
--                  -> Happy_GHC_Exts.Int#               -- input token
--                  -> HappyInput             -- input value being processed / "parse stack"
--                  -> Happy_GHC_Exts.Int#               -- st  : current state
--                  -> Happy_IntList          -- sts : state stack
--                  -> HappyStk HappyInput    -- stk : shift stack
--                  -> [HappyInput]           -- remaining input
--                  -> HappyIdentity HappyInput
happyMonadReduce k am nt fn 0# inp st sts stk
     = happyFail [] 0# inp st sts stk
happyMonadReduce k am nt fn j inp st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn sts stk) (\r -> happyGoto am nt j inp st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce v k am nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce v k am nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken v new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


-- happyGoto :: Verifying -- am
--           -> Happy_GHC_Exts.Int#       -- non-terminal on TOS
--           -> Happy_GHC_Exts.Int#       -- token int corresponding to the input
--           -> HappyInput     -- was tk, now inp
--           -> Happy_GHC_Exts.Int#       -- st
--           -> Happy_IntList -> HappyStk HappyInput
--           -> [HappyInput]
--           -> HappyIdentity HappyInput
happyGoto am nt j inp st =
   (happyTrace (", goto state " ++ show (Happy_GHC_Exts.I# (new_state)) ++ "\n")) $
   happyDoAction am j inp new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i
{-# LINE 764 "happy-templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
-- happyFail :: [String]
--           -> Happy_GHC_Exts.Int# -- input token value
--           -> HappyInput -- input
--           -> Happy_GHC_Exts.Int# -- current state
--           -> Happy_IntList
--           -> HappyStk HappyInput
--           -> [HappyInput]
--           -> HappyIdentity HappyInput
happyFail explist 0# inp old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { Node (Val{ here = HappyErrorToken (Happy_GHC_Exts.I# (i))}) _ -> i} ) in
        (happyTrace ( "happyFail:failing\n")) $
        happyError_ explist i inp

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts))
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction NotVerifying 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i inp (action) sts stk =
     (happyTrace ( "happyFail:entering error recovery\n")) $
   -- TODO:AZ: restore the error processing
        happyDoAction NotVerifying ((0#)) inp action sts ((mkNode (HappyErrorToken (Happy_GHC_Exts.I# (i))) Nothing False [] ) `HappyStk` stk)
        -- happyDoAction verifying    i                     inp new_state (HappyCons (st) (sts)) ( stk)
        -- happyError_ explist i inp

     -- = happyFail [] 0# tk st sts stk
-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
