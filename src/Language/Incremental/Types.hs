module Language.Incremental.Types
  (
   -- * Versioned document interface
    Versioned(..)
  , Node(..)
  , NodeScope(..)
  , VersionId(..)
  , nodeHasChanges
  , nodeChild
  , setChild
  , exists
  , isNew
  ) where

-- ---------------------------------------------------------------------
-- Node interface, from figure 2

{-
Summary of node-level interface used by the incremental parser. Each node
maintains its own version history, and is capable of reporting both local
changes to its attributes and “nested” changes—modifications within the subtree
rooted at the node. The version_id arguments refer to the document as a whole;
they are efficiently translated into names for values in the local history of
each versioned attribute.
-}

-- Will grow out to be the versioned document
data Versioned = Versioned
               deriving (Eq,Show)

data Node = Node -- TBD
               deriving (Eq,Show)

data NodeScope = Local | Nested
               deriving (Eq,Show)

data VersionId = VersionId Int -- for now
               deriving (Eq,Show)

-- Is this different from the VersionId?
data V = Reference | Previous | Current
               deriving (Eq,Show)

nodeHasChanges :: Versioned -> Maybe VersionId -> NodeScope -> Bool
nodeHasChanges doc mid scope = undefined

nodeChild :: Versioned -> Int -> Maybe VersionId -> Node
nodeChild n mid = undefined

setChild :: Versioned -> VersionId -> Node -> Versioned
setChild doc id node = undefined

-- | Determines whether the node exists in the current or a specified version.
exists :: Versioned -> Node -> VersionId -> Bool
exists doc node id = undefined

-- | Determines if a node was created in the current version.
isNew :: Versioned -> Node -> Bool
isNew doc node = undefined

-- ---------------------------------------------------------------------
{-
// Remove any subtrees on top of parse stack with null yield, then
// break down right edge of topmost subtree.
right_breakdown () {
  NODE *node;
  do { // Replace node with its children.
    node = parse_stack pop();
    // Does nothing when child is a terminal symbol.
    foreach child of node do shift(child);
  } while (is_nonterminal(node));
  shift(node); Leave final terminal symbol on top of stack.
}
-}

rightBreakdown = undefined


{-
// Shift a node onto the parse stack and update the current parse state.
void shift (NODE *node) {
  parse_stack push(parse_state, node);
  parse_state = parse_table state_after_shift(parse_state, node
}

-}

-- ---------------------------------------------------------------------
{-
void inc_parse () {
  // Initialize the parse stack to contain only bos.
  parse_stack clear(); parse_state = 0; parse_stack push(bos);
  NODE *la = pop_lookahead(bos); // Set lookahead to root of tree.
  while (true)
    if (is_terminal(la))
      // Incremental lexing advances la as a side effect.
      if (la has_changes(reference_version)) relex(la);
      else
        switch (parse_table action(parse_state, la symbol)) {
          case ACCEPT:
            if (la == eos) {
              parse_stack push(eos);
              return; Stack is [bos start_symbol eos].
            } else {recover(); break;}
          case REDUCE r: reduce(r); break;
          case SHIFT s: shift(s); la = pop_lookahead(la); break;
          case ERROR:
            recover(); break;
          }
    else // this is a nonterminal lookahead.
      if (la has_changes(reference_version)
        la = left_breakdown(la); Split tree at changed points.
      else {
        // Reductions can only be processed with a terminal lookahead.
        perform_all_reductions_possible(next_terminal());
        if (shiftable(la))
          // Place lookahead on parse stack with its right-hand edge removed.
          {shift(la); right_breakdown(); la = pop_lookahead(la);}
        else la = left_breakdown(la);
      }
}
-}

incParse = do
  -- parse_stack clear();
  -- parse_state = 0;
  -- parse_stack push(bos);

  NODE *la = pop_lookahead(bos); -- Set lookahead to root of tree.
  while (true)
    if (is_terminal(la))
      -- Incremental lexing advances la as a side effect.
      if (la has_changes(reference_version))
        relex(la);
      else
        switch (parse_table action(parse_state, la symbol)) {
          case ACCEPT:
            if (la == eos) {
              parse_stack push(eos);
              return; -- Stack is [bos start_symbol eos].
            } else {recover(); break;}
          case REDUCE r: reduce(r); break;
          case SHIFT s: shift(s); la = pop_lookahead(la); break;
          case ERROR:
            recover(); break;
          }
    else // this is a nonterminal lookahead.
      if (la has_changes(reference_version)
        la = left_breakdown(la); Split tree at changed points.
      else {
        // Reductions can only be processed with a terminal lookahead.
        perform_all_reductions_possible(next_terminal());
        if (shiftable(la))
          // Place lookahead on parse stack with its right-hand edge removed.
          {shift(la); right_breakdown(); la = pop_lookahead(la);}
        else la = left_breakdown(la);
      }

-- ---------------------------------------------------------------------

{-

-----------------------------------------------------------------------------
Info file generated by Happy Version 1.19.5 from app/Simple.y
-----------------------------------------------------------------------------


terminal 'C' is unused

-----------------------------------------------------------------------------
Grammar
-----------------------------------------------------------------------------
	%start_calc -> Top                                 (0)
	Top -> 'A' 'B'                                     (1)

-----------------------------------------------------------------------------
Terminals
-----------------------------------------------------------------------------
	'A'            { TokenA }
	'B'            { TokenB }
	'C'            { TokenC }

-----------------------------------------------------------------------------
Non-terminals
-----------------------------------------------------------------------------
	%start_calc     rule  0
	Top             rule  1

-----------------------------------------------------------------------------
States
-----------------------------------------------------------------------------
State 0


	'A'            shift, and enter state 2

	Top            goto state 3

State 1


	'A'            shift, and enter state 2


State 2

	Top -> 'A' . 'B'                                    (rule 1)

	'B'            shift, and enter state 4


State 3

	%start_calc -> Top .                                (rule 0)

	%eof           accept


State 4

	Top -> 'A' 'B' .                                    (rule 1)

	%eof           reduce using rule 1

-}

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

-- ---------------------------------------------------------------------

{-

%token
      'A'             { TokenA }
      'B'             { TokenB }
      'C'             { TokenC }

%%

Top   : 'A' 'B'  { Top TA TB }
-----------------------------

   | Action
   | A  B  eof | Top
S0 | S2 -  -   |  3
S1 | S2 -  -   |  -
S2 | -  S4 -   |  -
S3 | -  -  Ac  |  =
S4 | -  -  R1  |  3


So parse trace is

State  Symbol  Stack                 Action
0       'A'     (0,e)                Shift to state 2, accept 'A'
2       'B'     (0,'A') (4,'B')      Shift to state 4, accept 'B'
4       eof     (3,Top)              Accept


-}
