{-# LANGUAGE QuasiQuotes #-}
import qualified C.Parser as C
import qualified C.Compiler as C
import qualified Asm.Parser as Asm
import qualified Asm.Assembler as Asm
import qualified Asm.Preprocess as Asm
import qualified FORTH.Parser as FORTH
import qualified FORTH.Compiler as FORTH
import Asm.QQ
import qualified Data.ByteString.Lazy as B
import Control.Monad

main = do
    forthcode <- readFile "test.forth"
    header <- Asm.parseFile "ti84pcse.inc"

    let Right x = FORTH.compileText forthcode
    putStrLn . Asm.printTree $ x

    let file = do
        asm <- FORTH.compileText forthcode
        incs <- header
        bytes <- Asm.assemble $ incs ++ asm
        return $ Asm.makeFile "TESTPRG" Asm.EditLockedProg bytes



    {-ccode <- readFile "test.c"-}
    {-let tree = C.parseExpr ccode-}
    {-let asm = C.compileTree tree-}

    {-putStrLn . Asm.printTree $ asm-}

    {-header <- Asm.parseFile "ti84pcse.inc"-}
    {-let file = do-}
        {-incs <- header-}
        {-bytes <- Asm.assemble $ incs ++ asm-}
        {-return $ Asm.makeFile "TESTPRG" Asm.EditLockedProg bytes-}

    case file of
        Left err -> putStrLn $ "ERROR || " ++ err
        Right bytes -> do
            B.writeFile "TESTPRG.8xp" bytes
            putStrLn "Wrote File"

    -- asm <- Asm.assembleFile "testthing.z80" "TESTPRG" Asm.EditLockedProg
    -- header <- Asm.parseFile "ti84pcse.inc"
    -- let asm = do
    --     incs <- header
    --     bytes <- Asm.assemble $ incs ++ dcse ++ code 0x001F "UNKNOWN"
    --     return $ Asm.makeFile "TESTPRG" Asm.EditLockedProg bytes

    -- let asm = Asm.assemble (code 0xF800)
    -- putStrLn . Asm.printTree $ code 0x001F "UNKNOWN"
    -- print $ code 0x001F "UNKNOWN"
    -- case asm of
    --     Left err -> putStrLn $ "ERROR || " ++ err
    --     Right bytes -> do
    --         B.writeFile "TESTPRG.8xp" bytes
    --         putStrLn "Wrote file"
