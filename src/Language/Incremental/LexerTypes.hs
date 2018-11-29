{-# LANGUAGE StandaloneDeriving #-}
module Language.Incremental.LexerTypes
  (
    TokenL(..)
  , TokWrapper(..)
  , mkTok
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
data TokenL t
  = TokL
    { tokType      :: !(TokWrapper t) -- TokenType
    , tokLexeme    :: !String
    , tokState     :: !Int
    , tokLookAhead :: !Int
    , tokLookBack  :: !Int -- lookback is maintained externally
    , tokRelexed   :: !Bool -- TODO: Move to the enclosing Node.
    }

instance (Show t) => Show (TokenL t) where
  show (TokL t s st la lb rl)
    = unwords ["TokL",show t,show s,show st,show la,show lb,show rl]

deriving instance (Eq  t) =>  Eq (TokenL t)
deriving instance (Ord t) => Ord (TokenL t)

deriving instance (Eq  t)  =>   Eq (TokWrapper t)
deriving instance (Ord t)  =>  Ord (TokWrapper t)
deriving instance (Show t) => Show (TokWrapper t)

-- ---------------------------------------------------------------------

mkTok :: String -> t -> TokenL t
mkTok str tkn
  = TokL
      { tokType      = T tkn
      , tokLexeme    = str
      , tokState     = 0
      , tokLookAhead = 0
      , tokLookBack  = 0
      , tokRelexed   = True
      }
-- ---------------------------------------------------------------------

tokenLen :: TokenL t -> Int
tokenLen TokL {tokLexeme = l} = length l

-- ---------------------------------------------------------------------

bosToken :: TokenL t
bosToken = TokL TokenBos "" 0 0 0 True

eosToken :: TokenL t
eosToken = TokL TokenEos "" 0 0 0 False

-- ---------------------------------------------------------------------

isBosToken :: (Eq t) => TokenL t -> Bool
isBosToken TokL { tokType = tt } = tt == TokenBos

isEosToken :: (Eq t) => TokenL t -> Bool
isEosToken TokL { tokType = tt } = tt == TokenEos

-- ---------------------------------------------------------------------
