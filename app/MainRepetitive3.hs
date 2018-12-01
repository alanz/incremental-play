{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
import           Repetitive3

import           Control.Lens
import           Control.Zipper
import           Data.Tree
import           Data.Tree.Lens
import qualified Language.Incremental.LexerTypes as LT

main :: IO ()
main = do
-- main = getContents >>= print . calc . lexer
-- main = (print . calc . lexer) "AB"
  -- (print . calc . lexer) "1 + 2"
-- main = (print . pretty . calc . lexer) "1 + 2"
  -- let is = lexer' "1 + 2"
  -- putDoc $ pretty is
  putStr $ drawTree $ fmap show ptree

  putStrLn "--------------------------------"
  putStr $ drawTree $ fmap show newTree
  putStrLn "--------------------------------"

  let p' = calc [newTree]
  -- let p' = calc [ptree]
  putStr $ drawTree $ fmap show p'
  putStrLn "--------------------------------"
  -- putStr $ drawTree $ fmap show $ posify $ fmap toBar ptree
  putStr $ drawTree $ fmap show $ pp $ fmap toBar ptree
  putStrLn "--------------------------------"
  return ()

-- ptree :: HappyInput
ptree = (calc . lexer) "a BbDd c"

-- zipperTree :: Top :>> HappyInput
zipperTree = zipper ptree

foo :: IO ()
foo =
-- show
    -- zipperTree & downward root & view focus
    showTree newTree

{-
1 + 2 * 3
(Plus
  (Int 1)
  (Times (Int 2) (Int 3)))
-}
-- newTree :: Tree NodeVal
newTree = ptree
-- newTree =
--     zipperTree
--                & downward root & focus %~ setChangedChild & upward

--                & downward branches
--                & fromWithin traverse
--                & tugs rightward 1 -- HappyAbsSyn7
--                & downward root & focus %~ setChangedChild & upward

--                & downward branches
--                & fromWithin traverse
--                & downward root & focus %~ setChangedChild & upward

--                & downward branches
--                & fromWithin traverse
--                & tugs rightward 1
--                & downward root & focus %~ setChangedChild & upward

--                & downward branches
--                & fromWithin traverse
--                & tugs rightward 1
--                & downward root & focus %~ setChangedChild & upward

--                & downward branches
--                & fromWithin traverse
--                & tugs rightward 1
--                & downward root & focus %~ setChangedChild & upward

--                & downward branches
--                & fromWithin traverse
--                & tugs rightward 1
--                & downward root & focus %~ setChangedChild & upward

--                & downward branches
--                & fromWithin traverse
--                -- & tugs rightward 1
--                & downward root & focus %~ setChangedChild & upward

--                & downward branches
--                & fromWithin traverse
--                -- & tugs rightward 1
--                & downward root & focus %~ setChangedChild & upward

--                & downward branches
--                & fromWithin traverse
--                -- & tugs rightward 1
--                & downward root & focus %~ setChangedChild & upward

--                & downward root
--                -- & view focus
--                -- & focus %~ changeChild
--                & focus %~ changeVal
--                & rezip

-- changeVal :: NodeVal -> NodeVal
-- -- changeVal _ = Val True True (HappyErrorToken (-5)) Nothing [mkTok TokenBU ] Nothing Nothing False False False
changeVal _    = Val True True (HappyTerminal (tok)) Nothing [tok] Nothing Nothing False False False
  where
    tok = LT.mkTok "B" TokenBU

changeChild (Node v cs) = Node (changeVal v) []

-- setChangedChild :: NodeVal -> NodeVal
setChangedChild v = v { changedChild = True}

showTree :: Show a => Tree a -> IO ()
showTree tree = putStrLn $ drawTree $ fmap show tree

bar :: IO String
bar = fmap rezip $ zipper "stale" & within traverse <&> tugs rightward 2 <&> focus .~ 'y'

-- ---------------------------------------------------------------------

toBar :: (Show a) => Val a t -> (String, [t])
toBar v = (show (here v), terminals v)

-- ---------------------------------------------------------------------

data Bar = Bar { bLabel :: String
               , bToks :: [Tok]
               , bLength :: Int
               , bSpan :: Span
               } deriving Show

data Span = Span Int Int
            deriving Show

-- ---------------------------------------------------------------------

pp :: Tree (String,[Tok]) -> Tree Bar
pp t = head $ go (Span 0 0) [t]
  where
    go :: Span -> [Tree (String,[Tok])] -> [Tree Bar]
    go _ [] = []
    go sp@(Span s e) (Node i []:tts) = Node b []:go sp' tts
      where
        b = (ff sp i) { bSpan = sp' }
        sp' = Span e (e + bLength b)
    go sp (Node i ts:tts) = r:go sp' tts
      where
        b = (ff sp i) { bSpan = sp' }
        ts' = go sp ts
        r = Node b ts'
        Node bs _ = head ts'
        Node be _ = last ts'
        Span s _ = bSpan bs
        Span _ e = bSpan be
        sp' = Span s e

    ff (Span start end) (s,ts) = Bar s ts len sp
      where
        len = length ts
        sp = Span end (end + len)


-- ---------------------------------------------------------------------
