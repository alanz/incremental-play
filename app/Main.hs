-- import Simple
import ExprSimple
import Data.Text.Prettyprint.Doc

main :: IO ()
-- main = getContents >>= print . calc . lexer
-- main = (print . calc . lexer) "AB"
-- main = (print . calc . lexer) "1 + 2"
main = (print . pretty . calc . lexer) "1 + 2"
