{-# LANGUAGE OverloadedStrings #-}
-- import Simple
import           ExprSimple
-- import ExprSimpleOrig

import           Control.Applicative
import           Control.Lens
import           Control.Zipper
import           Data.Maybe
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO as TL
import           Data.Text.Prettyprint.Doc
import           Data.Tree
import           Data.Tree.Lens
import qualified System.Console.ANSI as ANSI
import           System.IO (Handle, stdout)
-- import Data.Text.Prettyprint.Doc.Render.Util.Panic
-- import Data.Text.Prettyprint.Doc.Render.Util.StackMachine
import           Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Text.Show.Prettyprint as PP

main :: IO ()
main = do
-- main = getContents >>= print . calc . lexer
-- main = (print . calc . lexer) "AB"
  -- (print . calc . lexer) "1 + 2"
-- main = (print . pretty . calc . lexer) "1 + 2"
  -- let is = lexer' "1 + 2"
  -- putDoc $ pretty is
  let p = (calc . lexer) "1 + 2"
  -- putDoc $ pretty p
  -- PP.prettyPrint p
  -- putStr $ drawTree $ fmap show p
  putStr $ drawTree $ fmap show ptree

  putStrLn "--------------------------------"

  let zipperTree = zipper p

  -- putStr $ show $ zipperTree
  return ()

ptree = (calc . lexer) "1 + 2"

zipperTree = zipper ptree

foo :: IO ()
foo = putStrLn $ show $
-- show
    -- zipperTree & downward root & view focus
    newTree

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
-- /show

changeVal :: NodeVal -> NodeVal
changeVal (Val cl cc h ts nt) = Val True True (HappyErrorToken (-5)) [mkTok TokenMinus ] Nothing

setChangedChild :: NodeVal -> NodeVal
setChangedChild v = v { changedChild = True}

showTree tree = putStrLn $ drawTree $ fmap show tree

bar :: IO String
bar = fmap rezip $ zipper "stale" & within traverse <&> tugs rightward 2 <&> focus .~ 'y'
