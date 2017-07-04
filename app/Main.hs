-- import Simple
import ExprSimple

main :: IO ()
-- main = getContents >>= print . calc . lexer
-- main = (print . calc . lexer) "AB"
main = (print . calc . lexer) "1 + 2"
