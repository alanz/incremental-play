import Simple

main :: IO ()
-- main = getContents >>= print . calc . lexer
main = (print . calc . lexer) "AB"
