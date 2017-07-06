{-# LANGUAGE OverloadedStrings #-}
-- import Simple
import ExprSimple
-- import Data.Text.Prettyprint.Doc
-- import Data.Text.Prettyprint.Doc.Render.Terminal

import           Control.Applicative
import           Data.Maybe
import           Data.Semigroup
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO      as TL
import qualified System.Console.ANSI    as ANSI
import           System.IO              (Handle, stdout)

import Data.Text.Prettyprint.Doc
-- import Data.Text.Prettyprint.Doc.Render.Util.Panic
-- import Data.Text.Prettyprint.Doc.Render.Util.StackMachine
import Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Text.Show.Prettyprint as PP

main :: IO ()
main = do
-- main = getContents >>= print . calc . lexer
-- main = (print . calc . lexer) "AB"
-- main = (print . calc . lexer) "1 + 2"
-- main = (print . pretty . calc . lexer) "1 + 2"
  let p = (calc . lexer) "1 + 2"
  -- (putDoc . pretty . calc . lexer) "1 + 2"
  putDoc $ pretty p
  PP.prettyPrint p
  return ()


-- foo = do
--   Data.Text.Prettyprint.Doc.Render.Terminal.putDoc (bold "hello" <+> bold "world")

foo = putDoc styledDoc

-- style = color Green <> bold
style = bold
styledDoc = annotate style "hello world"

w1 :: Doc AnsiStyle
w1 = "foo"

bar = renderIO System.IO.stdout (layoutPretty defaultLayoutOptions "hello\nworld")
bar2 = renderIO System.IO.stdout (layoutPretty defaultLayoutOptions styledDoc)
