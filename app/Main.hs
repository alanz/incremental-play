{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
-- import Simple
import           ExprSimple
-- import ExprSimpleOrig

-- import           Control.Applicative
import           Control.Lens
import           Control.Zipper
-- import           Data.Maybe
-- import           Data.Semigroup
-- import           Data.Text (Text)
-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL
-- import qualified Data.Text.Lazy.Builder as TLB
-- import qualified Data.Text.Lazy.IO as TL
-- import           Data.Text.Prettyprint.Doc
import           Data.Tree
import           Data.Tree.Lens
-- import qualified System.Console.ANSI as ANSI
-- import           System.IO (Handle, stdout)
-- import Data.Text.Prettyprint.Doc.Render.Util.Panic
-- import Data.Text.Prettyprint.Doc.Render.Util.StackMachine
-- import           Data.Text.Prettyprint.Doc.Render.Terminal
-- import qualified Text.Show.Prettyprint as PP

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
  putStr $ drawTree $ fmap show p'
  return ()

ptree :: HappyInput
ptree = (calc . lexer) "1 + 2"

zipperTree :: Top :>> HappyInput
zipperTree = zipper ptree

foo :: IO ()
foo = putStrLn $ show $
-- show
    -- zipperTree & downward root & view focus
    newTree

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

               & downward root
               -- -- & view focus
               & focus %~ changeVal
               & rezip

changeVal :: NodeVal -> NodeVal
changeVal _ = Val True True (HappyErrorToken (-5)) [mkTok TokenMinus ] Nothing

setChangedChild :: NodeVal -> NodeVal
setChangedChild v = v { changedChild = True}

showTree :: Show a => Tree a -> IO ()
showTree tree = putStrLn $ drawTree $ fmap show tree

bar :: IO String
bar = fmap rezip $ zipper "stale" & within traverse <&> tugs rightward 2 <&> focus .~ 'y'
