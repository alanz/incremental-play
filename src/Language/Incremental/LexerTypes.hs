{-# LANGUAGE StandaloneDeriving #-}
module Language.Incremental.LexerTypes
  (
    Token(..)
  , TokWrapper(..)
  , tokenLen
  , bosToken
  , eosToken
  , isBosToken
  , isEosToken
  ) where

-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------

-- TODO: should this wrapper belong to the node instead? as well?
data TokWrapper t = TokenBos -- ^ bos sentinel according to the paper
                  | T t      -- ^ an actual Token
                  | TokenEos -- ^ eos sentinel according to the paper

-- The token type
data Token t
  = Tok
    { tokType      :: !(TokWrapper t) -- TokenType
    , tokLexeme    :: !String
    , tokState     :: !Int
    , tokLookAhead :: !Int
    , tokLookBack  :: !Int -- lookback is maintained externally
    , tokRelexed   :: !Bool -- TODO: Move to the enclosing Node.
    }

instance (Show t) => Show (Token t) where
  show (Tok t s st la lb rl)
    = unwords ["Tok",show t,show s,show st,show la,show lb,show rl]

deriving instance (Eq  t) =>  Eq (Token t)
deriving instance (Ord t) => Ord (Token t)

deriving instance (Eq  t)  =>   Eq (TokWrapper t)
deriving instance (Ord t)  =>  Ord (TokWrapper t)
deriving instance (Show t) => Show (TokWrapper t)

-- ---------------------------------------------------------------------

tokenLen :: Token t -> Int
tokenLen Tok {tokLexeme = l} = length l

-- ---------------------------------------------------------------------

bosToken :: Token t
bosToken = Tok TokenBos "" 0 0 0 True

eosToken :: Token t
eosToken = Tok TokenEos "" 0 0 0 False

-- ---------------------------------------------------------------------

isBosToken :: (Eq t) => Token t -> Bool
isBosToken Tok { tokType = tt } = tt == TokenBos

isEosToken :: (Eq t) => Token t -> Bool
isEosToken Tok { tokType = tt } = tt == TokenEos

-- ---------------------------------------------------------------------
