import C.Parser
import C.Compiler

main = do
    txt <- readFile "test.c"
    putStrLn $ compileTree (parseExpr txt)
