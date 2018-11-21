{-# LANGUAGE StandaloneDeriving #-}
module Language.Incremental.LexerTypes
  (
    Token(..)
  ) where

-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------

-- The token type
data Token t
  = Tok
    { tokType      :: t -- TokenType
    , tokLexeme    :: String
    , tokState     :: Int
    , tokLookAhead :: Int
    , tokLookBack  :: Int -- lookback is maintained externally
    }

instance (Show t) => Show (Token t) where
  show (Tok t s st la lb) = unwords ["Tok",show t,show s,show st,show la,show lb]

deriving instance (Eq  t) => Eq  (Token t)
deriving instance (Ord t) => Ord (Token t)
