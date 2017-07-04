{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
-- This file is (initially) based on the example in happy manual
-- https://www.haskell.org/happy/doc/html/sec-using.html
module ExprSimple where

import Data.Char
import qualified Data.Bits as Bits
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import qualified System.IO as Happy_System_IO
import qualified System.IO.Unsafe as Happy_System_IO_Unsafe
import qualified Debug.Trace as Happy_Debug_Trace
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10


-- array (4,10) [(4,Nothing),(5,Nothing),(6,Nothing),(7,Nothing),(8,Nothing),(9,Nothing),(10,Nothing)]


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x18\x00\x60\x00\x00\x00\x00\x00\x04\x00\x04\x00\x04\x00\x04\x00\x00\x00\x60\x00\x60\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_calc","Ultraroot","bos","eos","tree","Exp","Term","Factor","int","'+'","'-'","'*'","'/'","%eof"]
        bit_start = st * 16
        bit_end = (st + 1) * 16
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..15]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x00\x00\x13\x00\x0f\x00\x00\x00\x0c\x00\x08\x00\x00\x00\x00\x00\x12\x00\x12\x00\x12\x00\x12\x00\x00\x00\x08\x00\x08\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x0a\x00\x11\x00\xfe\xff\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\xff\xff\x03\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xfd\xff\x00\x00\x00\x00\x00\x00\xfc\xff\xfb\xff\xf8\xff\xf5\xff\xf4\xff\x00\x00\x00\x00\x00\x00\x00\x00\xfe\xff\xf9\xff\xfa\xff\xf6\xff\xf7\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x03\x00\x04\x00\x05\x00\x06\x00\x06\x00\x05\x00\x06\x00\x05\x00\x06\x00\x00\x00\x01\x00\x04\x00\x05\x00\x02\x00\x03\x00\x02\x00\x06\x00\x01\x00\x01\x00\x01\x00\x06\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x04\x00\x05\x00\x06\x00\x07\x00\x10\x00\x0e\x00\x07\x00\x0f\x00\x07\x00\x03\x00\x02\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0d\x00\x11\x00\x02\x00\x09\x00\x09\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 11) [
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
	(11 , happyReduce_11)
	]

happy_n_terms = 7 :: Int
happy_n_nonterms = 7 :: Int

happyReduce_1 am = happySpecReduce_3  am 0# happyReduction_1
happyReduction_1 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 am = happySpecReduce_0  am 1# happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 (()
	)

happyReduce_3 am = happySpecReduce_0  am 2# happyReduction_3
happyReduction_3  =  HappyAbsSyn6
		 (()
	)

happyReduce_4 am = happySpecReduce_1  am 3# happyReduction_4
happyReduction_4 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 am = happySpecReduce_3  am 4# happyReduction_5
happyReduction_5 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 am = happySpecReduce_3  am 4# happyReduction_6
happyReduction_6 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 am = happySpecReduce_1  am 4# happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (Term happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 am = happySpecReduce_3  am 5# happyReduction_8
happyReduction_8 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Times happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 am = happySpecReduce_3  am 5# happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Div happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 am = happySpecReduce_1  am 5# happyReduction_10
happyReduction_10 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Factor happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 am = happySpecReduce_1  am 6# happyReduction_11
happyReduction_11 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn10
		 (Int happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	happyDoAction Normal (Terminal 6#) notHappyAtAll action sts stk []

happyNewToken action sts stk (t:ts) =
	let cont i tk = happyDoAction am (Terminal i) tk action sts stk ts
	    am = Normal in
	case t of {
	  InputToken tk ->
	    case tk of {
		TokenInt happy_dollar_dollar -> cont 1# tk;
		TokenPlus -> cont 2# tk;
		TokenMinus -> cont 3# tk;
		TokenTimes -> cont 4# tk;
		TokenDiv -> cont 5# tk;
		_ -> happyError' ((t:ts), [])
		};

	_ -> error "to be implemented";

	};

happyError_ explist 6# tk tks = happyError' (tks, explist)
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
happyError' :: () => ([(ParserInput HappyAbsSynType)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
calc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "happy-templates/GenericTemplate.hs" #-}
{-# LINE 1 "happy-templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 12 "<command-line>" #-}
{-# LINE 1 "/usr/include/stdc-predef.h" #-}

{-# LINE 17 "/usr/include/stdc-predef.h" #-}













































{-# LINE 12 "<command-line>" #-}
{-# LINE 1 "/opt/ghc/8.0.2/lib/ghc-8.0.2/include/ghcversion.h" #-}

















{-# LINE 12 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc12600_0/ghc_2.h" #-}




























































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































{-# LINE 12 "<command-line>" #-}
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







{-# LINE 74 "happy-templates/GenericTemplate.hs" #-}

{-# LINE 84 "happy-templates/GenericTemplate.hs" #-}



happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO $ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    return expr












infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-- AZ: following to come out of happy ProduceCode
type HappyAbsSynType = HappyAbsSyn Exp () () Exp Exp Term Factor

type HappyInput = ParserInput HappyAbsSynType

data DoACtionMode = Normal | AllReductions
                  deriving Eq

-----------------------------------------------------------------------------
-- starting the parse

happyParse :: Happy_GHC_Exts.Int# -> [HappyInput] -> HappyIdentity HappyAbsSynType
happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

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



data Input a
  = Terminal Happy_GHC_Exts.Int#
  | NonTerminal a

-- AZ:NOTE: The second param below (Token) can/should be moved into the Input
-- type, as it is meaningless for a nonterminal. But what about compatibility
-- with other happy options?
--
-- The problem comes from the mapping of a Token to a unique number in
-- happyNewToken
--
-- For now, keep it outside, but give an error value when processing a NonTerminal
-- This leads to the unfortunate creation of a second input type.
data ParserInput a
  = InputToken Token
  | InputTree a


-- old: happyDoAction :: TokenId -> Token -> State -> StateStack -> ItemStack -> [Tokens]
happyDoAction :: DoACtionMode
              -> Input HappyAbsSynType-> Token
              -> Happy_GHC_Exts.Int# -- ^ Current state
              -> Happy_IntList -> HappyStk HappyAbsSynType -- Current state and shifted item stack
              -> [HappyInput] -- Input being processed
              -> HappyIdentity HappyAbsSynType
happyDoAction mode inp tk st
  = case mode of
    Normal ->
      case inp of
        Terminal i ->
          (happyTrace ("state: " ++ show (Happy_GHC_Exts.I# (st)) ++                       ",\ttoken: " ++ show (Happy_GHC_Exts.I# (i)) ++                       ",\taction: ")) $


          case action of
                0#           -> (happyTrace ("fail.\n")) $
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> (happyTrace ("accept.\n")) $
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> (happyTrace ("reduce (rule " ++ show rule                                                                ++ ")")) $

                                                   (happyReduceArr Happy_Data_Array.! rule) Normal i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> (happyTrace ("shift, enter state "                                                  ++ show (Happy_GHC_Exts.I# (new_state))                                                  ++ "\n")) $


                                     happyShift new_state inp tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
          where off    = indexShortOffAddr happyActOffsets st
                off_i  = (off Happy_GHC_Exts.+# i)
                check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                         then EQ(indexShortOffAddr happyCheck off_i, i)
                         else False
                action
                 | check     = indexShortOffAddr happyTable off_i
                 | otherwise = indexShortOffAddr happyDefActions st
        NonTerminal tree ->
          (happyTrace ("state: " ++ show (Happy_GHC_Exts.I# (st)) ++                       ",\ttree: TBD" ++                       ",\taction: ")) $


          (performAllReductionsPossible (next_terminal) tk st)
    AllReductions -> performAllReductionsPossible (next_terminal) tk st

next_terminal = Terminal (0#)

performAllReductionsPossible :: Input HappyAbsSynType-> Token
              -> Happy_GHC_Exts.Int# -- ^ Current state
              -> Happy_IntList -> HappyStk HappyAbsSynType -- Current state and shifted item stack
              -> [HappyInput] -- Input being processed
              -> HappyIdentity HappyAbsSynType
performAllReductionsPossible inp tk st
    = case inp of
        Terminal i ->
          (happyTrace ("reduceAll:state: " ++ show (Happy_GHC_Exts.I# (st)) ++                       ",\ttoken: " ++ show (Happy_GHC_Exts.I# (i)) ++                       ",\taction: ")) $


          case action of
                0#           -> (happyTrace ("fail.\n")) $
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> (happyTrace ("accept.\n")) $
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> (happyTrace ("reduce (rule " ++ show rule                                                                ++ ")")) $

                                                   (happyReduceArr Happy_Data_Array.! rule) AllReductions i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> (happyTrace ("shift, enter state "                                                  ++ show (Happy_GHC_Exts.I# (new_state))                                                  ++ "\n")) $


                                     happyShift new_state inp tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
          where off    = indexShortOffAddr happyActOffsets st
                off_i  = (off Happy_GHC_Exts.+# i)
                check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                         then EQ(indexShortOffAddr happyCheck off_i, i)
                         else False
                action
                 | check     = indexShortOffAddr happyTable off_i
                 | otherwise = indexShortOffAddr happyDefActions st
        NonTerminal _tree -> error "performAllReductionsPossible NonTerminal"



-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 269 "happy-templates/GenericTemplate.hs" #-}


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

{-# LINE 309 "happy-templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift :: Happy_GHC_Exts.Int#
           -> Input HappyAbsSynType
           -> Token
           -> Happy_GHC_Exts.Int#
           -> Happy_IntList
           -> HappyStk HappyAbsSynType
           -> [HappyInput]
           -> HappyIdentity HappyAbsSynType
happyShift new_state ((Terminal (0#))) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction Normal ((Terminal (i))) tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 :: DoACtionMode
                  -> Happy_GHC_Exts.Int#
                  -> HappyAbsSynType
                  -> Happy_GHC_Exts.Int#
                  -> Token
                  -> Happy_GHC_Exts.Int#
                  -> Happy_IntList
                  -> HappyStk HappyAbsSynType
                  -> [HappyInput]
                  -> HappyIdentity HappyAbsSynType
happySpecReduce_0 am i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 am nt fn j tk st@((action)) sts stk
     = happyGoto am nt ((Terminal (j))) tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 :: DoACtionMode
                  -> Happy_GHC_Exts.Int#
                  -> (HappyAbsSynType -> HappyAbsSynType)
                  -> Happy_GHC_Exts.Int#
                  -> Token
                  -> Happy_GHC_Exts.Int#
                  -> Happy_IntList
                  -> HappyStk HappyAbsSynType
                  -> [HappyInput]
                  -> HappyIdentity HappyAbsSynType
happySpecReduce_1 am i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 am nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto am nt ((Terminal (j))) tk st sts (r `HappyStk` stk'))

happySpecReduce_2 :: DoACtionMode
                  -> Happy_GHC_Exts.Int#
                  -> (HappyAbsSynType -> HappyAbsSynType -> HappyAbsSynType)
                  -> Happy_GHC_Exts.Int#
                  -> Token
                  -> Happy_GHC_Exts.Int#
                  -> Happy_IntList
                  -> HappyStk HappyAbsSynType
                  -> [HappyInput]
                  -> HappyIdentity HappyAbsSynType
happySpecReduce_2 am i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 am nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto am nt ((Terminal (j))) tk st sts (r `HappyStk` stk'))

happySpecReduce_3 :: DoACtionMode
                  -> Happy_GHC_Exts.Int#
                  -> (HappyAbsSynType -> HappyAbsSynType -> HappyAbsSynType -> HappyAbsSynType)
                  -> Happy_GHC_Exts.Int#
                  -> Token
                  -> Happy_GHC_Exts.Int#
                  -> Happy_IntList
                  -> HappyStk HappyAbsSynType
                  -> [HappyInput]
                  -> HappyIdentity HappyAbsSynType
happySpecReduce_3 am i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 am nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto am nt ((Terminal (j))) tk st sts (r `HappyStk` stk'))

happyReduce k am i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k am nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto am nt ((Terminal (j))) tk st1 sts1 r)

happyMonadReduce k am nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k am nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto am nt ((Terminal (j))) tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k am nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k am nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto :: DoACtionMode
          -> Happy_GHC_Exts.Int# -> Input HappyAbsSynType -> Token -> Happy_GHC_Exts.Int#
          -> Happy_IntList -> HappyStk HappyAbsSynType
          -> [HappyInput]
          -> HappyIdentity HappyAbsSynType
happyGoto am nt j tk st =
   (happyTrace (", goto state " ++ show (Happy_GHC_Exts.I# (new_state)) ++ "\n")) $
   happyDoAction am j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i
{-# LINE 457 "happy-templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail :: [String]
          -> Happy_GHC_Exts.Int#
          -> Token
          -> Happy_GHC_Exts.Int#
          -> Happy_IntList
          -> HappyStk HappyAbsSynType
          -> [HappyInput]
          -> HappyIdentity HappyAbsSynType
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $
        happyError_ explist i ((InputToken (tk)))

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction Normal 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction Normal ((Terminal (0#))) tk action sts ( (HappyErrorToken (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

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
