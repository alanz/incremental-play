{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Language.Incremental.Lexer
  (
    update_lookbacks
    -- Token(..)
  , fixLookBacks
  , fixLookBacks'
  ) where

import Data.FingerTree
import Data.List
import Data.List.Zipper as Z
import Data.Monoid
import Language.Incremental.LexerTypes
-- import Language.Incremental.ParserTypes

-- ---------------------------------------------------------------------

-- import Debug.Trace
-- debug = flip trace
-- debug c str = c

-- ---------------------------------------------------------------------

type TokenTree t = FingerTree (Sum Int) (TokenL t)

instance Measured (Sum Int) (TokenL t) where
  measure _ = 1

data LaSetItem t = LA { laTok :: TokenL t, laCla :: Int, laCnt :: Int }
               deriving (Eq)

instance Show (LaSetItem t) where
  show = pLaSetItem

pLaSetItem :: LaSetItem t -> String
pLaSetItem (LA t cla cnt)
  = "(" ++ show (tokLexeme t) ++ ","++ show cla ++ "," ++ show cnt ++ ")"

-- pls :: [LaSetItem t] -> String
-- pls ls = intercalate ",\n" $ map pLaSetItem ls

-- ---------------------------------------------------------------------

-- update_lookbacks = carry on here
update_lookbacks = undefined
{-
// Find and update each modified region of tokens.
void update_lookbacks () {
  NODE *node = root;
  while (node)
    if (is_token(node)) {
      TOKEN *tok = (TOKEN*)node;
      if (was_re_lexed(tok)) node = fix_lookbacks(tok);
      else node = next_subtree(node);
    } else if (node->has_changes(nested)) node = node->child(0);
    else node = next_subtree(node);
}
Figure 5.14: Driver routine for lookback recomputation.
-}
-- ---------------------------------------------------------------------

fixLookBacks' :: (Eq t, Ord t, Show t) => [TokenL t] -> [TokenL t]
fixLookBacks' toks
  = (toList . fixLookBacks . Z.fromList) $ addSentinels toks

addSentinels :: [TokenL t] -> [TokenL t]
addSentinels toks = (bosToken:toks) ++ [eosToken]

-- ---------------------------------------------------------------------

-- | Given a token tree split at the point of interest, so the first
-- token we care about is the start of tokr
fixLookBacks :: forall t. (Eq t,Ord t,Show t) => Zipper (TokenL t) -> Zipper (TokenL t)
fixLookBacks tokz = tokz'
  where
    -- Extract lookback count (if different in current version, use
    -- old value).
    -- TODO:bring in ref implementation
    maybeOldTokz = tokz
    lb = tokLookBack (cursor maybeOldTokz)

    -- The boot_tok is the furthest back we should go in the lookback region.
    boot_tok = go_boot lb maybeOldTokz
      where
        go_boot 0 tz = tz
        go_boot l tz
          | Z.beginp tz = tz
          | otherwise   = go_boot (l - 1) (Z.left tz)

     -- Initialize the lookahead set from the bootstrap region.
    la_set :: [LaSetItem t]
    la_set = go_la [] boot_tok
      where
        go_la :: [LaSetItem t] -> Zipper (TokenL t) -> [LaSetItem t]
        go_la acc tz
          | cursor tz == cursor tokz = acc
          | otherwise = go_la (add_item (cursor tz) (advance acc len)) (Z.right tz)
              where
                len = tokenLen $ cursor tz

    -- Set the lookback for re-lexed tokens.
    (la_lb,tz_lb) = go_lb la_set tokz
      where
        go_lb :: [LaSetItem t] -> Zipper (TokenL t) -> ([LaSetItem t], Zipper (TokenL t))
        go_lb la tz
          | tokRelexed (cursor tz) = go_lb la' (next_token tz')
          | otherwise = (la,tz)
            where
              (la',tz') = processLookback la tz

    -- Symmetric to bootstrap: process unmodified tokens reached by
    -- lookahead from re-lexed area.
    (la_rl,tz_rl) = go_lb la_lb tz_lb
      where
        go_lb la tz
          | not (isEosToken tok) &&
            not (tokRelexed tok) &&
            not (all_items_discardable la) &&
            (tokLookBack tok /= compute_lookback la)
             = go_lb la' (next_token tz')
          | otherwise = (la,tz)
            where
              tok = cursor tz
              (la',tz') = processLookback la tz

    tokz' = tz_rl


-- ---------------------------------------------------------------------

{-
      tok->lookback = la_set.compute_lookback();
      la_set.advance(tok->length);
      la_set.add_item(tok);
      tok = next_token(tok);
-}
processLookback :: (Show t)
                => [LaSetItem t]
                     -> Zipper (TokenL t) -> ([LaSetItem t], Zipper (TokenL t))
processLookback la tz = (la',tz')
  where
    lb = compute_lookback la
    t = cursor tz
    t' = t { tokLookBack = lb }

    la' = add_item t' (advance la (tokenLen t))

    tz' = replace t' tz

-- ---------------------------------------------------------------------

-- advance (int offset)
--   replace <tok,cla,cnt> in list with <tok,cla - offset,cnt + 1>
advance :: [LaSetItem t] -> Int -> [LaSetItem t]
advance la_set offset = r
  where
    r = filter positiveCla $ fmap advance_tok la_set
    advance_tok :: LaSetItem t -> LaSetItem t
    advance_tok (LA tok cla cnt) = LA tok (cla - offset) (cnt + 1)
    positiveCla (LA _ cla _) = cla > 0

-- ---------------------------------------------------------------------

-- int compute_lookback ()
--   remove <tok,cla,cnt> s.t. cla <= 0 from list
--   if (list == {}) return 0;
--   else return max cnt | <tok,cla,cnt> in list
compute_lookback :: (Show t) => [LaSetItem t] -> Int
compute_lookback la = r
  where
    r = foldl' check 0 la
    check acc (LA _tok cla' cnt')
      | cla' > 0 = max cnt' acc
      | otherwise = acc

-- ---------------------------------------------------------------------
-- add_item (TOKEN *tok)
--   add <tok,tok->lookahead,0> to list
add_item :: TokenL t -> [LaSetItem t] -> [LaSetItem t]
add_item tok la
  = if tokLookAhead tok > 0
      then LA tok (tokLookAhead tok) 1 : la
      else LA tok (tokLookAhead tok) 0 : la

-- ---------------------------------------------------------------------

-- bool all_items_discardable ()
--  forall <tok,cla,cnt> in list, !was_re_lexed(tok)
all_items_discardable :: [LaSetItem t] -> Bool
all_items_discardable la = all (not . tokRelexed . laTok) la

-- ---------------------------------------------------------------------

-- | The token stream should always have a BOS token at the beginning as a sentinel.
previous_token :: Zipper (TokenL t) -> Zipper (TokenL t)
previous_token tz
  | endp (left tz) = tz
  | otherwise = left tz

-- | The token stream should always have a EOS token at the end as a sentinel.
next_token :: Zipper (TokenL t) -> Zipper (TokenL t)
next_token tz
  | endp (right tz) = tz
  | otherwise = right tz

-- ---------------------------------------------------------------------
{-
P65 Figure 5.15

// Process a re-lexed region starting at tok .
TOKEN *fix_lookbacks (TOKEN *tok) {
  la_set = {};
  if (tok != bos) {
    // Extract lookback count (if different in current version, use old value).
    int lb = next_token(previous_token(tok), reference_version)->lookback;
    TOKEN *boot_tok = tok;
    while (--lb > 0 &&
          previous_token(boot_tok, reference_version) ==
            previous_token(boot_tok, previous_version) &&
          !was_re_lexed(previous_token(boot_tok)))
      boot_tok = previous_token(boot_tok);
    // Initialize the lookahead set from the bootstrap region.
    while (boot_tok != tok) {
      la_set.advance(tok->length);
      la_set.add_item(tok);
      tok = next_token(tok);
    }
  }
  do {
    // Set the lookback for re-lexed tokens.
    while (was_re_lexed(tok)) {
      tok->lookback = la_set.compute_lookback();
      la_set.advance(tok->length);
      la_set.add_item(tok);
      tok = next_token(tok);
    }
    // Symmetric to bootstrap: process unmodified tokens reached by lookahead from re-lexed area.
    while (tok != eos && !was_re_lexed(tok) &&
           !la_set.all_items_discardable() &&
           tok->lookback != la_set.compute_lookback()) {
      tok->lookback = la_set.compute_lookback();
      la_set.advance(tok->length);
      la_set.add_item(tok);
      tok = next_token(tok);
    }
  } while (was_re_lexed(tok));
  return tok; // Return first clean token or 'eos' to caller.
}


// P65 Figure 5.16

advance (int offset)
  replace <tok,cla,cnt> in list with <tok,cla - offset,cnt + 1>

int compute_lookback ()
  remove <tok,cla,cnt> s.t. cla <= 0 from list
  if (list == {}) return 0;
  else return max cnt | <tok,cla,cnt> in list

add_item (TOKEN *tok)
  add <tok,tok->lookahead,0> to list

bool all_items_discardable ()
 forall <tok,cla,cnt> in list, !was_re_lexed(tok)



-}

-- ---------------------------------------------------------------------
