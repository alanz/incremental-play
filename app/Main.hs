import Simple

main :: IO ()
main = getContents >>= print . calc . lexer
