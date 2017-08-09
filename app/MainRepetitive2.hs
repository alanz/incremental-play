{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
import           Repetitive2

import           Control.Lens
import           Control.Zipper
import           Data.Tree
import           Data.Tree.Lens

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
newTree :: Tree NodeVal
newTree =
    zipperTree
               & downward root & focus %~ setChangedChild & upward

               & downward branches
               & fromWithin traverse
               & tugs rightward 1 -- HappyAbsSyn7
               & downward root & focus %~ setChangedChild & upward

               & downward branches
               & fromWithin traverse
               & downward root & focus %~ setChangedChild & upward

               & downward branches
               & fromWithin traverse
               & tugs rightward 1
               & downward root & focus %~ setChangedChild & upward

               & downward branches
               & fromWithin traverse
               & tugs rightward 1
               & downward root & focus %~ setChangedChild & upward

               & downward branches
               & fromWithin traverse
               & tugs rightward 1
               & downward root & focus %~ setChangedChild & upward

               & downward branches
               & fromWithin traverse
               & tugs rightward 1
               & downward root & focus %~ setChangedChild & upward

               & downward branches
               & fromWithin traverse
               -- & tugs rightward 1
               & downward root & focus %~ setChangedChild & upward

               & downward branches
               & fromWithin traverse
               -- & tugs rightward 1
               & downward root & focus %~ setChangedChild & upward

               & downward branches
               & fromWithin traverse
               -- & tugs rightward 1
               & downward root & focus %~ setChangedChild & upward

               & downward root
               -- & view focus
               -- & focus %~ changeChild
               & focus %~ changeVal
               & rezip

changeVal :: NodeVal -> NodeVal
-- changeVal _ = Val True True (HappyErrorToken (-5)) Nothing [mkTok TokenBU ] Nothing Nothing False False False
changeVal _    = Val True True (HappyTerminal TokenBU) Nothing [mkTok TokenBU] Nothing Nothing False False False

changeChild (Node v cs) = Node (changeVal v) []

setChangedChild :: NodeVal -> NodeVal
setChangedChild v = v { changedChild = True}

showTree :: Show a => Tree a -> IO ()
showTree tree = putStrLn $ drawTree $ fmap show tree

bar :: IO String
bar = fmap rezip $ zipper "stale" & within traverse <&> tugs rightward 2 <&> focus .~ 'y'
