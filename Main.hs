import C.Parser
import C.Compiler
import qualified Asm.Parser as Asm
import qualified Asm.Assembler as Asm
import qualified Asm.Preprocess as Asm
import qualified Data.ByteString.Lazy as B

main = do
    asm <- Asm.assembleFile "testthing.z80" "TESTPRG" Asm.Prog
    case asm of
        Left err -> putStrLn $ "ERROR || " ++ err
        Right bytes -> do
            B.writeFile "TESTPRG.8xp" bytes
            putStrLn "Wrote file"

    -- let out = Asm.printTree tree
    -- putStrLn $ unlines $ map ('\t':) $ lines out
