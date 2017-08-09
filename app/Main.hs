{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
-- import Simple
import           ExprSimple
-- import ExprSimpleOrig

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
  putStr $ drawTree $ fmap show newTree2
  putStrLn "--------------------------------"

  let p' = calc [newTree2]
  putStr $ drawTree $ fmap show p'
  return ()

-- ptree :: HappyInput
ptree = (calc . lexer) "1 + 2"

-- zipperTree :: Top :>> HappyInput
zipperTree = zipper ptree

foo :: IO ()
foo = putStrLn $ show $
-- show
    -- zipperTree & downward root & view focus
    newTree

-- newTree :: Tree NodeVal
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

               & downward root
               -- -- & view focus
               & focus %~ changeVal
               & rezip

-- newTree2 :: Tree NodeVal
newTree2 =
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
               & tugs rightward 2
               & downward root & focus %~ setChangedChild & upward

               & downward branches
               & fromWithin traverse
               & downward root & focus %~ setChangedChild & upward

               & downward branches
               & fromWithin traverse
               & downward root & focus %~ setChangedChild & upward

               & downward root
               -- & view focus
               & focus %~ changeVal2
               & rezip

-- changeVal :: NodeVal -> NodeVal
changeVal _ = Val True True (HappyErrorToken (-5)) Nothing [mkTok TokenMinus ] Nothing Nothing False False False

-- changeVal2 :: NodeVal -> NodeVal
changeVal2 _ = Val True True (HappyTerminal (TokenInt 3)) Nothing [mkTok (TokenInt 3) ] (Just (mkTok (TokenInt 3))) (Just (mkTok (TokenInt 3))) False False False

-- setChangedChild :: NodeVal -> NodeVal
setChangedChild v = v { changedChild = True}

showTree :: Show a => Tree a -> IO ()
showTree tree = putStrLn $ drawTree $ fmap show tree

bar :: IO String
bar = fmap rezip $ zipper "stale" & within traverse <&> tugs rightward 2 <&> focus .~ 'y'
