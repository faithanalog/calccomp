import C.Parser
import C.Compiler
import qualified Asm.Parser as Asm
import qualified Asm.Assembler as Asm
import qualified Asm.Preprocess as Asm
import qualified Data.ByteString.Lazy as B

main = do
    text <- Asm.preprocess "testthing.z80"
    let tree = Asm.parseExpr text
    let asm = Asm.assemble tree
    case asm of
        Left err -> putStrLn $ "ERROR || " ++ err
        Right bytes -> do
            B.writeFile "TESTPRG.8xp" bytes
            putStrLn "Wrote file"

    -- let out = Asm.printTree tree
    -- putStrLn $ unlines $ map ('\t':) $ lines out
