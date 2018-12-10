-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

#ifdef HAPPY_GHC
#undef __GLASGOW_HASKELL__
#define HAPPY_IF_GHC_GT_500 #if __GLASGOW_HASKELL__ > 500
#define HAPPY_IF_GHC_GE_503 #if __GLASGOW_HASKELL__ >= 503
#define HAPPY_ELIF_GHC_500 #elif __GLASGOW_HASKELL__ == 500
#define HAPPY_IF_GHC_GT_706 #if __GLASGOW_HASKELL__ > 706
#define HAPPY_ELSE #else
#define HAPPY_ENDIF #endif
#define HAPPY_DEFINE #define
#endif

#ifdef HAPPY_GHC
#define ILIT(n) n#
#define IBOX(n) (Happy_GHC_Exts.I# (n))
#define FAST_INT Happy_GHC_Exts.Int#
-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
HAPPY_IF_GHC_GT_706
HAPPY_DEFINE LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
HAPPY_DEFINE GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
HAPPY_DEFINE EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
HAPPY_ELSE
HAPPY_DEFINE LT(n,m) (n Happy_GHC_Exts.<# m)
HAPPY_DEFINE GTE(n,m) (n Happy_GHC_Exts.>=# m)
HAPPY_DEFINE EQ(n,m) (n Happy_GHC_Exts.==# m)
HAPPY_ENDIF
#define PLUS(n,m) (n Happy_GHC_Exts.+# m)
#define MINUS(n,m) (n Happy_GHC_Exts.-# m)
#define TIMES(n,m) (n Happy_GHC_Exts.*# m)
#define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))
#define IF_GHC(x) (x)
#else
#define ILIT(n) (n)
#define IBOX(n) (n)
#define FAST_INT Int
#define LT(n,m) (n < m)
#define GTE(n,m) (n >= m)
#define EQ(n,m) (n == m)
#define PLUS(n,m) (n + m)
#define MINUS(n,m) (n - m)
#define TIMES(n,m) (n * m)
#define NEGATE(n) (negate (n))
#define IF_GHC(x)
#endif

data Happy_IntList = HappyCons FAST_INT Happy_IntList

#if defined(HAPPY_ARRAY)
#define CONS(h,t) (HappyCons (h) (t))
#else
#define CONS(h,t) ((h):(t))
#endif

#if defined(HAPPY_INCR)
#  define ERROR_TOK ILIT(0)
#  define DO_ACTION(verifying,state,i,tk,sts,stk) happyDoAction verifying i tk state sts (stk)
#  define HAPPYSTATE(i) (i)
#  define GOTO(action) happyGoto
#  define IF_ARRAYS(x) (x)
#  define NODE(x) (Node (x))
#elif defined(HAPPY_ARRAY)
#  define ERROR_TOK ILIT(0)
#  define DO_ACTION(state,i,tk,sts,stk) happyDoAction i tk state sts (stk)
#  define HAPPYSTATE(i) (i)
#  define GOTO(action) happyGoto
#  define IF_ARRAYS(x) (x)
#  define NODE(x) (x)
#else
#  define ERROR_TOK ILIT(1)
#  define DO_ACTION(state,i,tk,sts,stk) state i i tk HAPPYSTATE(state) sts (stk)
#  define HAPPYSTATE(i) (HappyState (i))
#  define GOTO(action) action
#  define IF_ARRAYS(x)
#  define NODE(x) (x)
#endif

#if defined(HAPPY_COERCE)
#define GET_ERROR_TOKEN(x)  (case Happy_GHC_Exts.unsafeCoerce# x of { IBOX(i) -> i })
#define MK_ERROR_TOKEN(i)   (Happy_GHC_Exts.unsafeCoerce# IBOX(i))
#define MK_TOKEN(x)         (happyInTok (x))
#elif defined(HAPPY_INCR)
#define GET_ERROR_TOKEN(x)  (case x of { Node (Val{ here = HappyErrorToken IBOX(i)}) _ -> i} )
#define MK_ERROR_TOKEN(i)   (mkNode (HappyErrorToken IBOX(i)) Nothing False [] )
#define MK_TOKEN(x)         (mkNode (HappyTerminal (x)) [])
#else
#define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken IBOX(i) -> i })
#define MK_ERROR_TOKEN(i)   (HappyErrorToken IBOX(i))
#define MK_TOKEN(x)         (HappyTerminal (x))
#endif

#if defined(HAPPY_DEBUG)
#define DEBUG_TRACE(s)    (happyTrace (s)) $
happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO $ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    return expr
#else
#define DEBUG_TRACE(s)    {- nothing -}
#endif

#if defined(HAPPY_INCR)
#define TOK_ERR_INT ILIT(-10)
mkTok t = Tok TOK_ERR_INT t
-- nullTok = Tok TOK_ERR_INT notHappyAtAll
-- nullTok = Tok TOK_ERR_INT TokenPlus
-- nullTok = Tok TOK_ERR_INT TokenA
#define TERMINAL(i) (i)
#define TOKEN(i) ((i))
#else
#define TERMINAL(i) (i)
#define TOKEN(i) (i)
#endif

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

#define HAPPYSTATESENTINEL  (ILIT(-1000))
happyNodeSentinel = mkNode (HappyErrorToken (-1000)) Nothing False []

-- happyParse :: FAST_INT -> [HappyInput] -> HappyIdentity HappyInput
happyParse start_state = happyNewToken NotVerifying start_state CONS(HAPPYSTATESENTINEL,notHappyAtAll) (happyNodeSentinel `HappyStk` notHappyAtAll)

-- showStacks :: Happy_IntList -> HappyStk HappyInput -> String
showStacks (CONS(HAPPYSTATESENTINEL,_)) _ = "[]"
showStacks (CONS(st,sts)) ((Node v _) `HappyStk` stks)
  = show (IBOX(st),take 40 $ showHere v) ++ ":" ++ showStacks sts stks

-- showInputQ :: [HappyInput] -> String
showInputQ is = "[" ++ intercalate "," (map (showHere . rootLabel) is) ++ "]"

-- showInput :: [HappyInput] -> String
showInput ts = "[" ++ intercalate "," (map (showHere . rootLabel) ts) ++ "]"

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept ERROR_TOK tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
        IF_GHC(happyTcHack j IF_ARRAYS(happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays in incremental mode: do the next action

#if defined(HAPPY_INCR)

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

-- data Tok = Tok FAST_INT Token
--   deriving Show
-- instance Pretty Tok

-- type HappyInput = Node HappyAbsSynType Tok

mkTokensNode tks = setTerminals (mkNode (HappyErrorToken (-5)) Nothing False []) tks

setTerminals :: Node a b -> [b] -> Node a b
setTerminals (Node v cs) ts = Node (v { terminals = ts}) cs

getTerminals :: Node a b -> [b]
getTerminals (Node v cs) = terminals v

-- happyDoAction :: Verifying
--               -> FAST_INT   -- ^ Current lookahead token number
--               -> HappyInput -- ^ input being processed. "parse stack" from the paper  Same as first item on input list?
--               -> FAST_INT   -- ^ Current state
--               -> Happy_IntList -> HappyStk HappyInput -- ^ Current state and shifted item stack
--               -> [HappyInput] -- ^ Input being processed
--               -> HappyIdentity HappyInput
happyDoAction verifying la inp@(Node v@(Val {terminals = toks, next_terminal = mnext, here_nt = mnt}) cs) st sts stk tks
  = DEBUG_TRACE("--------------------------\n")
    DEBUG_TRACE("happyDoAction:verifying=" ++ show verifying ++ "\n")
    DEBUG_TRACE("happyDoAction:la=" ++ show IBOX(la) ++ "\n")
    DEBUG_TRACE("happyDoAction:inp=" ++ showHere v ++ "\n")
    DEBUG_TRACE("happyDoAction:stacks=" ++ showStacks sts stk ++ "\n")
    DEBUG_TRACE("happyDoAction:tks=" ++ showInputQ tks ++ "\n")
    case toks of -- Terminals
      (tok@(Tok i tk):ts) ->
        DEBUG_TRACE("t:state: " ++ show IBOX(st) ++
                    ",\tfragile: " ++ show fragile ++
                    ",\ttoken: " ++ show IBOX(i) ++
                    ",\taction: ")
        case action of
              ILIT(0)   -> DEBUG_TRACE("fail.\n")
                           if verifying == Verifying
                             then rightBreakdown st sts stk tks
                             else happyFail (happyExpListPerState (IBOX(st) :: Int)) i inp st sts stk tks
              ILIT(-1)  -> DEBUG_TRACE("accept. A\n")
                             happyAccept i tk st sts stk tks
              n | LT(n,(ILIT(0) :: FAST_INT))
                        -> DEBUG_TRACE("reduce (rule " ++ show rule ++ ")")
                           (happyReduceArr Happy_Data_Array.! rule) NotVerifying fragile i inp st sts stk tks
                            where rule = IBOX(NEGATE(PLUS(n,(ILIT(1) :: FAST_INT))))
              n         -> DEBUG_TRACE("shift, enter state "
                                       ++ show IBOX(new_state)
                                       ++ "\n")
                           happyShift NotVerifying new_state i (mkNodeNt (HappyTerminal tk) Nothing fragile [] tok) st sts stk tks
                           where new_state = MINUS(n,(ILIT(1) :: FAST_INT))
        where action = lookupAction st i
              fragile = happyFragileState IBOX(st)
      _ -> -- Non-terminal input
        DEBUG_TRACE("nt:state: " ++ show IBOX(st) ++
                    ",\tfragile: " ++ show (happyFragileState IBOX(st)) ++
                    ",\ttree: " ++ (take 35 $ show (here $ rootLabel inp)) ++
                    ",\taction: ")
        if changed inp || isFragile inp
          then DEBUG_TRACE ("left breakdown.\n")
               leftBreakdown verifying la inp st sts stk tks
          else
            case mnt of
              Just (IBOX(i)) ->
                DEBUG_TRACE("nt:" ++ show (IBOX(i)) ++ ",actionv:" ++ show (IBOX(action)) ++ ":")
-------------------------------
                case action of
                      ILIT(0)           -> DEBUG_TRACE("fail.\n")
                                           if null cs
                                             then happyNewToken verifying st sts stk tks
                                             else leftBreakdown NotVerifying la inp st sts stk tks
                      ILIT(-1)          -> DEBUG_TRACE("nt:accept. A\n")
                                             -- This can never happen
                                             notHappyAtAll
                      n | LT(n,(ILIT(0) :: FAST_INT)) -> DEBUG_TRACE("reduce (rule " ++ show rule
                                                                     ++ ")")
                                                         (happyReduceArr Happy_Data_Array.! rule) NotVerifying fragile i inp st sts stk tks
                                                         where rule = IBOX(NEGATE(PLUS(n,(ILIT(1) :: FAST_INT))))
                      n                 -> DEBUG_TRACE("shift, enter state "
                                                       ++ show IBOX(new_state)
                                                       ++ "\n")
                                           happyShift Verifying new_state i (Node v' cs) st sts stk tks
                                           where new_state = MINUS(n,(ILIT(1) :: FAST_INT))
                                                 v' = v { grammarFragile = fragile }
                where action = lookupAction st i
                      fragile = happyFragileState IBOX(st)
-------------------------------
              Nothing -> DEBUG_TRACE ("mnext == Nothing.\n")
                          happyNewToken NotVerifying st sts stk tks


-- leftBreakdown :: Verifying
--               -> FAST_INT   -- ^ Current lookahead token number
--               -> HappyInput -- ^ input being processed. "parse stack" from the paper Same as first item on input list?
--               -> FAST_INT   -- ^ Current state
--               -> Happy_IntList -> HappyStk HappyInput -- ^ Current state and shifted item stack
--               -> [HappyInput] -- ^ Input being processed
--               -> HappyIdentity HappyInput
leftBreakdown verifying la inp@(Node v cs) st sts stk ts
  = DEBUG_TRACE("leftBreakdown:ts=" ++ showInputQ ts ++ "\n")
    DEBUG_TRACE("leftBreakdown:inp=" ++ showHere v ++ "\n")
    case cs of
      []    -> DEBUG_TRACE("leftBreakdown:no children\n")
               -- happyNewToken verifying st sts stk ts
               -- happyNewToken verifying st sts stk (inp':ts)
               happyDoAction verifying la inp' st sts stk ts
               where inp' = Node (v { changedLocal = False, changedChild = False}) cs
      (c:cs') -> if isFragile c
                   then DEBUG_TRACE("leftBreakdown:fragile:" ++ showHere (rootLabel c) ++ "\n")
                        leftBreakdown verifying la c st sts stk (cs' ++ ts)
                   else DEBUG_TRACE("leftBreakdown:not fragile\n")
                        happyNewToken verifying      st sts stk (cs  ++ ts)

-- rightBreakdown :: FAST_INT   -- ^ Current state
--                -> Happy_IntList -> HappyStk HappyInput -- ^ Current state and shifted item stack
--                -> [HappyInput] -- ^ Input being processed
--                -> HappyIdentity HappyInput
rightBreakdown st sts@(CONS(sts1,stss)) stk@(stk1@(Node v cs) `HappyStk` stks)
  -- Break down the right hand edge of the top of the parse stack until it is
  -- the last_terminal value of the original.
  -- Nodes not having a last_terminal are discarded (no yield)
  = DEBUG_TRACE("rightBreakdown:stacks=" ++ (showStacks sts stk) ++ "\n")
    if hasYield stk1
     then case cs of
            [] -> DEBUG_TRACE("rightBreakdown:has yield, no children, ie token:(st,sts1,stk1)=" ++
                              (unwords [show IBOX(st),show IBOX(sts1), take 30 ( show (here v))]) ++ ".\n")
                  case last_terminal v of
                    Just (Tok i _) ->
                      case (nextStateShift sts1 i) of
                        Just (IBOX(st2)) -> DEBUG_TRACE("rightBreakdown:nextStateShift:" ++ show (IBOX(sts1),IBOX(i),IBOX(st2)) ++ "\n")
                                            happyNewToken NotVerifying st2 sts stk
                        Nothing          -> notHappyAtAll
                    Nothing       -> DEBUG_TRACE("rightBreakdown:no nt\n")
                                     happyNewToken NotVerifying sts1 sts stk
            _  -> -- shift each child onto the stack, then call rightBreakdown again
              DEBUG_TRACE("rightBreakdown:going through children (n=" ++ show (length cs) ++ ").\n")
              rightBreakdown st2 sts' stk'
              where
                (st',sts',stk') = foldl' go (IBOX(sts1),stss,stks) cs
                !(IBOX(st2)) = st'
                -- go :: (Int, Happy_IntList, HappyStk HappyInput) -> HappyInput -> (Int, Happy_IntList, HappyStk HappyInput)
                go  (IBOX(st), sts, stk) c@(Node v@(Val {last_terminal = mtok,here_nt = mnt}) _)
                  = DEBUG_TRACE("rightBreakdown:go:(st,v)=" ++ show (IBOX(st),take 30 $ showHere v) ++ "\n")
                    case (mnt, mtok) of
                      (Just (IBOX(nt)), Just (Tok i tk)) ->
                        DEBUG_TRACE("go:nt " ++ (showStacks sts stk) ++ "\n")
                        (IBOX(nextState st nt), CONS(st,sts), (c `HappyStk` stk))
                      (Nothing,         Just (Tok i tk)) ->
                        DEBUG_TRACE("go:terminal " ++ (showStacks sts stk) ++ "\n")
                        (IBOX(nextState st i), CONS(st,sts), (c `HappyStk` stk))
                      _ -> DEBUG_TRACE("rightBreakdown:no non-terminal and/or no last_terminal.\n") notHappyAtAll

     else DEBUG_TRACE("rightBreakdown,no yield, popping stack") rightBreakdown sts1 stss stks

lookupAction' :: Int -> Int -> Int
lookupAction' st' i' =
  case (st',i') of
   (IBOX(st), IBOX(i)) -> (IBOX(lookupAction st i))

lookupAction :: FAST_INT -> FAST_INT -> FAST_INT
lookupAction st i = action
        where off    = indexShortOffAddr happyActOffsets st
              off_i  = PLUS(off,i)
              check  = if GTE(off_i,(ILIT(0) :: FAST_INT))
                       then EQ(indexShortOffAddr happyCheck off_i, i)
                       else False
              action
               | check     = indexShortOffAddr happyTable off_i
               | otherwise = indexShortOffAddr happyDefActions st


nextState' :: Int -> Int -> Int
nextState' st' nt' =
  case (st',nt') of
   (IBOX(st), IBOX(nt)) -> (IBOX(nextState st nt))

nextState :: FAST_INT -> FAST_INT -> FAST_INT
nextState st nt =
  if happyGotoValid IBOX(st) IBOX(nt)
    then new_state
    else ILIT(0)
  where off = indexShortOffAddr happyGotoOffsets st
        off_i = PLUS(off,nt)
        new_state = indexShortOffAddr happyTable off_i

nextStateShift' :: Int -> Int -> Maybe Int
nextStateShift' st' i' =
  case (st',i') of
    ((IBOX(st)), (IBOX(i))) -> nextStateShift st i

nextStateShift :: FAST_INT -> FAST_INT -> Maybe Int
nextStateShift st i =
  if (GTE(action, (ILIT(1) :: FAST_INT)))
    then Just IBOX(MINUS(action,(ILIT(1) :: FAST_INT)))
    else Nothing
    -- else Just IBOX(action)
  where off    = indexShortOffAddr happyActOffsets st
        off_i  = PLUS(off,i)
        check  = if GTE(off_i,(ILIT(0) :: FAST_INT))
                 then EQ(indexShortOffAddr happyCheck off_i, i)
                 else False
        action :: FAST_INT
        action
         | check     = indexShortOffAddr happyTable off_i
         | otherwise = indexShortOffAddr happyDefActions st

-- changed :: HappyInput -> Bool
changed (Node (Val { changedLocal = cl, changedChild = cc}) _) = cl || cc

-- hasYield :: HappyInput -> Bool
hasYield (Node (Val { last_terminal = mlt}) _) = isJust mlt
#endif /* HAPPY_INCR */


-----------------------------------------------------------------------------
-- Arrays only: do the next action

#if defined(HAPPY_ARRAY) && !defined(HAPPY_INCR)

happyDoAction i tk st
        = DEBUG_TRACE("state: " ++ show IBOX(st) ++
                      ",\ttoken: " ++ show IBOX(i) ++
                      ",\taction: ")
          case action of
                ILIT(0)           -> DEBUG_TRACE("fail.\n")
                                     happyFail (happyExpListPerState (IBOX(st) :: Int)) i tk st
                ILIT(-1)          -> DEBUG_TRACE("accept. C\n")
                                     happyAccept i tk st
                n | LT(n,(ILIT(0) :: FAST_INT)) -> DEBUG_TRACE("reduce C (rule " ++ show rule
                                                               ++ ")")
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = IBOX(NEGATE(PLUS(n,(ILIT(1) :: FAST_INT))))
                n                 -> DEBUG_TRACE("shift, enter state "
                                                 ++ show IBOX(new_state)
                                                 ++ "\n")
                                     happyShift new_state i tk st
                                     where new_state = MINUS(n,(ILIT(1) :: FAST_INT))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = PLUS(off,i)
         check  = if GTE(off_i,(ILIT(0) :: FAST_INT))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st

#endif /* HAPPY_ARRAY */

#ifdef HAPPY_GHC
indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#
#else
indexShortOffAddr arr off = arr Happy_Data_Array.! off
#endif

#ifdef HAPPY_GHC
readArrayBit arr bit =
    Bits.testBit IBOX(indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#)) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x
#else
readArrayBit arr bit =
    Bits.testBit IBOX(indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)
#endif

#ifdef HAPPY_GHC
data HappyAddr = HappyA# Happy_GHC_Exts.Addr#
#endif

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

#if !defined(HAPPY_ARRAY)

newtype HappyState b c = HappyState
        (FAST_INT ->                    -- token number
         FAST_INT ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)

#endif

-----------------------------------------------------------------------------
-- Shifting a token

-- happyShift :: Verifying
--            -> FAST_INT  -- new state
--            -> FAST_INT  --  Current lookahead token number
--            -> HappyInput -- current input / "parse tree"
--            -> FAST_INT   -- current state
--            -> Happy_IntList
--            -> HappyStk HappyInput
--            -> [HappyInput]
--            -> HappyIdentity HappyInput
happyShift verifying new_state (TERMINAL(ERROR_TOK)) inp st sts stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
--     trace "shifting the error token" $
     DO_ACTION(verifying,new_state,i,inp,CONS(st,sts),stk)

happyShift verifying new_state i inp st sts stk =
     -- DEBUG_TRACE("happyShift:(new_state,i,inp)=" ++ show (IBOX(new_state),IBOX(i),inp) ++ "\n")
     happyNewToken verifying new_state CONS(st,sts) (inp `HappyStk`stk)

-- happyReduce is specialised for the common cases.

-- happySpecReduce_0 :: Verifying
--                   -> FAST_INT   -- Non terminal to end up on TOS
--                   -> HappyInput -- function from TOS items to new TOS
--                   -> FAST_INT   -- input token value
--                   -> HappyInput
--                   -> FAST_INT
--                   -> Happy_IntList
--                   -> HappyStk HappyInput
--                   -> [HappyInput]
--                   -> HappyIdentity HappyInput
happySpecReduce_0 am nt fn ERROR_TOK inp st sts stk
     = happyFail [] ERROR_TOK inp st sts stk
happySpecReduce_0 am nt fn j inp st@(HAPPYSTATE(action)) sts stk
     = GOTO(action) am nt j inp st CONS(st,sts) (fn `HappyStk` stk)

-- happySpecReduce_1 :: Verifying
--                   -> FAST_INT
--                   -> (HappyInput -> HappyInput)
--                   -> FAST_INT
--                   -> HappyInput
--                   -> FAST_INT
--                   -> Happy_IntList
--                   -> HappyStk HappyInput
--                   -> [HappyInput]
--                   -> HappyIdentity HappyInput
happySpecReduce_1 am i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_1 am nt fn j tk _ sts@(CONS(st@HAPPYSTATE(action),_)) (v1`HappyStk`stk')
     = let !r = fn v1 in -- TODO:AZ strictness?
       happySeq r (GOTO(action) am nt j tk st sts (r `HappyStk` stk'))

-- happySpecReduce_2 :: Verifying
--                   -> FAST_INT
--                   -> (HappyInput -> HappyInput -> HappyInput)
--                   -> FAST_INT
--                   -> HappyInput
--                   -> FAST_INT
--                   -> Happy_IntList
--                   -> HappyStk HappyInput
--                   -> [HappyInput]
--                   -> HappyIdentity HappyInput
happySpecReduce_2 am i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_2 am nt fn j tk _ CONS(_,sts@(CONS(st@HAPPYSTATE(action),_))) (v1`HappyStk`v2`HappyStk`stk')
     = let !r = fn v1 v2 in -- TODO:AZ strictness?
       happySeq r (GOTO(action) am nt j tk st sts (r `HappyStk` stk'))

-- happySpecReduce_3 :: Verifying
--                   -> FAST_INT
--                   -> (HappyInput -> HappyInput -> HappyInput -> HappyInput)
--                   -> FAST_INT
--                   -> HappyInput
--                   -> FAST_INT
--                   -> Happy_IntList
--                   -> HappyStk HappyInput
--                   -> [HappyInput]
--                   -> HappyIdentity HappyInput
happySpecReduce_3 am i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_3 am nt fn j tk _ CONS(_,CONS(_,sts@(CONS(st@HAPPYSTATE(action),_)))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let !r = fn v1 v2 v3 in -- TODO:AZ strictness?
       happySeq r (GOTO(action) am nt j tk st sts (r `HappyStk` stk'))

happyReduce k am i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyReduce k am nt fn j tk st sts stk
     = case happyDrop MINUS(k,(ILIT(1) :: FAST_INT)) sts of
         sts1@(CONS(st1@HAPPYSTATE(action),_)) ->
                let !r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (GOTO(action) am nt j tk st1 sts1 r)

-- happyMonadReduce :: FAST_INT      -- number of items to remove from stack
--                  -> Verifying
--                  -> FAST_INT
--                  -> (Happy_IntList -> HappyStk HappyInput -> HappyIdentity HappyInput)
--                  -> FAST_INT               -- input token
--                  -> HappyInput             -- input value being processed / "parse stack"
--                  -> FAST_INT               -- st  : current state
--                  -> Happy_IntList          -- sts : state stack
--                  -> HappyStk HappyInput    -- stk : shift stack
--                  -> [HappyInput]           -- remaining input
--                  -> HappyIdentity HappyInput
happyMonadReduce k am nt fn ERROR_TOK inp st sts stk
     = happyFail [] ERROR_TOK inp st sts stk
happyMonadReduce k am nt fn j inp st sts stk =
      case happyDrop k CONS(st,sts) of
        sts1@(CONS(st1@HAPPYSTATE(action),_)) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn sts stk) (\r -> GOTO(action) am nt j inp st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce v k am nt fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyMonad2Reduce v k am nt fn j tk st sts stk =
      case happyDrop k CONS(st,sts) of
        sts1@(CONS(st1@HAPPYSTATE(action),_)) ->
         let drop_stk = happyDropStk k stk
#if defined(HAPPY_ARRAY)
             off = indexShortOffAddr happyGotoOffsets st1
             off_i = PLUS(off,nt)
             new_state = indexShortOffAddr happyTable off_i
#else
             new_state = action
#endif
          in
          happyThen1 (fn stk tk) (\r -> happyNewToken v new_state sts1 (r `HappyStk` drop_stk))

happyDrop ILIT(0) l = l
happyDrop n CONS(_,t) = happyDrop MINUS(n,(ILIT(1) :: FAST_INT)) t

happyDropStk ILIT(0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk MINUS(n,(ILIT(1)::FAST_INT)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

#if defined(HAPPY_INCR)
-- happyGoto :: Verifying -- am
--           -> FAST_INT       -- non-terminal on TOS
--           -> FAST_INT       -- token int corresponding to the input
--           -> HappyInput     -- was tk, now inp
--           -> FAST_INT       -- st
--           -> Happy_IntList -> HappyStk HappyInput
--           -> [HappyInput]
--           -> HappyIdentity HappyInput
happyGoto am nt j inp st =
   DEBUG_TRACE(", goto state " ++ show IBOX(new_state) ++ "\n")
   happyDoAction am j inp new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = PLUS(off,nt)
         new_state = indexShortOffAddr happyTable off_i
#elif defined(HAPPY_ARRAY)
happyGoto nt j tk st =
   DEBUG_TRACE(", goto state " ++ show IBOX(new_state) ++ "\n")
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = PLUS(off,nt)
         new_state = indexShortOffAddr happyTable off_i
#else
happyGoto action j tk st = action j j tk (HappyState action)
#endif

-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
-- happyFail :: [String]
--           -> FAST_INT -- input token value
--           -> HappyInput -- input
--           -> FAST_INT -- current state
--           -> Happy_IntList
--           -> HappyStk HappyInput
--           -> [HappyInput]
--           -> HappyIdentity HappyInput
happyFail explist ERROR_TOK inp old_st _ stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
        DEBUG_TRACE( "happyFail:failing\n")
        happyError_ explist i inp

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts)
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(NotVerifying,action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i inp@(Node v@(Val {terminals = toks}) cs) HAPPYSTATE(action) sts stk =
  let
          inp1 = case toks of
            [] -> inp
            (Tok _ t:ts) -> Node v {terminals = Tok ERROR_TOK t : ts} cs
  in
     DEBUG_TRACE( "happyFail:entering error recovery\n")
   -- TODO:AZ: restore the error processing
        DO_ACTION(NotVerifying,action,(TERMINAL(ERROR_TOK)),inp1,sts,MK_ERROR_TOKEN(i) `HappyStk` stk)
        -- DO_ACTION(verifying   ,new_state,i                    ,inp,CONS(st,sts), stk)
        -- happyError_ explist i inp

     -- = happyFail [] ERROR_TOK tk st sts stk
-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

#if defined(HAPPY_GHC)
happyTcHack :: FAST_INT -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}
#endif

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

#if defined(HAPPY_ARRAY)
{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}
#endif
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
