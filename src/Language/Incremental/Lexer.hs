{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Incremental.Lexer
  (
    -- Token(..)
    fixLookBacks
  , fixLookBacks'
  ) where

import Language.Incremental.LexerTypes
import Data.FingerTree
import qualified Data.Set as Set
import Data.Monoid
import Data.List.Zipper as Z

-- ---------------------------------------------------------------------

type TokenTree t = FingerTree (Sum Int) (Token t)

instance Measured (Sum Int) (Token t) where
  measure _ = 1

-- ---------------------------------------------------------------------

fixLookBacks' :: (Eq t, Ord t ) => [Token t] -> [Token t]
fixLookBacks' toks = (toList . fixLookBacks . Z.fromList) toks

-- ---------------------------------------------------------------------

-- | Given a token tree split at the point of interest, so the first
-- token we care about is the start of tokr
fixLookBacks :: (Eq t,Ord t,Ord (Token t)) => Zipper (Token t) -> Zipper (Token t)
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
    la_set = go_la Set.empty boot_tok
      where
        -- go_la :: (Eq t) => Set.Set a -> Zipper (Token t) -> _
        go_la acc tz
          | cursor tz == cursor tokz = acc
          | otherwise = go_la (Set.insert (cursor tz) (advance acc len))  (Z.right tz)
              where
                len = length $ tokLexeme $ cursor tz
    tokz' = tokz


-- advance (int offset)
--   replace <tok,cla,cnt> in list with <tok,cla - offset,cnt + 1>
advance :: (Ord t) => Set.Set (Token t) -> Int -> Set.Set (Token t)
advance la_set offset = Set.map (advance_tok offset) la_set

advance_tok :: Int -> Token t -> Token t
advance_tok offset t@(Tok { tokLookAhead = la, tokLookBack = lb })
  = t { tokLookAhead = la - offset, tokLookBack = lb + 1 }

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
  else return max cnt |v<tok,cla,cnt> in list

add_item (TOKEN *tok)
  add <tok,tok->lookahead,0> to list

bool all_items_discardable ()
 forall <tok,cla,cnt> in list, !was_re_lexed(tok)



-}

-- ---------------------------------------------------------------------
