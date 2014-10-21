{-# LANGUAGE QuasiQuotes #-}
module FORTH.Compiler (compileExprs, compileText) where

import FORTH.Parser
import FORTH.Stdlib
import Asm.QQ
import Data.Maybe
import Data.List
import qualified Asm.Expr as A
import qualified Asm.Parser as A
import qualified Data.Map.Strict as Map


type Strings = Map.Map String String
type Vars    = [String]

getStrings :: [Expr] -> Strings
getStrings xprs
    | null strs = Map.empty
    | otherwise = Map.fromList [strPair x | x <- [0..length strs - 1]]
    where strs = findStrs xprs
          strPair x = (strs !! x, "STRCNST_" ++ show x)
          findStrs xs = [x | (Str x) <- xs] ++ concat [findStrs x | (WordDef _ x) <- xs]

getVars :: [Expr] -> Vars
getVars xprs = [x | (VarDef x) <- xprs] ++ concat [getVars x | (WordDef _ x) <- xprs]

getDepends :: [Expr] -> [Dependency]
getDepends xprs = nub . concat $ map depends rtns
    where toks = [t | (Tok t) <- xprs]
          rtns = mapMaybe stdlibAsm toks

wordPrologue nm = [asm|
    @{"WDEF_" ++ nm ++ ":"}
    pop hl
    dec ix
    ld (ix),h
    dec ix
    ld (ix),l
    @{"RWDEF_" ++ nm ++ ":"} ;For recursing
|]

wordEpilogue = [asm|
    ld l,(ix)
    inc ix
    ld h,(ix)
    inc ix
    jp (hl)
|]

defWord :: Strings -> Vars -> Expr -> [A.Expr]
defWord strs vars (WordDef nm body) = wordPrologue nm ++ code ++ wordEpilogue
    where code = concatMap (compileBody strs vars) body
          compileBody strs vars x = case x of
              Tok "RECURSE" -> [asm|jp @{"RWDEF_" ++ nm}|]
              _ -> compileExpr strs vars x

defWordAsm :: Expr -> [A.Expr]
defWordAsm (WordDefAsm nm body) = wordPrologue nm ++ code ++ wordEpilogue
    where code = case A.parseText "" body of
              Left err -> error err
              Right xs -> xs

defString :: (String, String) -> [A.Expr]
defString (s,lbl) = [asm|@{lbl ++ ":"} \ .db @s{s},0|]

defVar :: String -> [A.Expr]
defVar v = [asm|.var word, @{"VAR_" ++ v}|]

compileExpr :: Strings -> Vars -> Expr -> [A.Expr]
compileExpr _    _ (Num x) = [asm|ld hl,@{x} \ push hl|]
compileExpr strs _ (Str s) = let Just lbl = Map.lookup s strs in [asm|ld hl,@{lbl} \ push hl|]
compileExpr _ vars (Tok t) = case stdlibAsm t of
                             Nothing -> if t `elem` vars then
                                                         [asm|ld hl,@{"VAR_" ++ t} \ push hl|]
                                                         else
                                                         [asm|call @{"WDEF_" ++ t}|]
                             Just x -> code x
compileExpr _ _ _ = []

asmStart "ti84pcse" = [asm|
    CODE_START:
    .org UserMem - 2
    .db tExtTok, tAsm84CCmp
    .varloc 4000h, 3800h     ;Leave 800h for R stack
    oldStack = plotSScreen + 768 - 2
    oldPage  = plotSScreen + 768 - 3
    ld (oldStack),sp
    in a,(6)
    ld (oldPage),a
    ld a,87h
    out (6),a
    ld ix,4000h + 4000h      ;R stack is 800h
    ld sp,saveSScreen + 300h ;Prog stack is 300h
|]

asmStart "ti83p" = [asm|
    CODE_START:
    .org UserMem - 2
    .db t2ByteTok, tAsmCmp
    .varloc appBackupScreen, 300h
    .var word, oldStack
    ld (oldStack),sp
    ld ix,statVars + 531     ;R stack is 531
    ld sp,saveSScreen + 300h ;Prog stack is 300h
|]

asmEnd "ti84pcse" = [asm|
    ld sp,(oldStack)
    ld a,(oldPage)
    out (6),a
    ret
    CODE_END:
|]

asmEnd "ti83p" = [asm|
    ld sp,(oldStack)
    b_call(_DelRes)
    ret
    CODE_END:
|]

compileExprs :: [Expr] -> [A.Expr]
compileExprs xprs = concatMap concat [
        map defVar vars,
        code,
        [defWord strings vars w | w@WordDef{} <- xprs],
        [defWordAsm w | w@WordDefAsm{} <- xprs],
        [c | (Dependency c) <- getDepends xprs],
        map defString (Map.toList strings)
    ]
    where strings = getStrings xprs
          vars = getVars xprs
          code = [asmStart platform] ++ map (compileExpr strings vars) xprs ++ [asmEnd platform]
          platform = "ti84pcse"

compileText :: String -> Either String [A.Expr]
compileText str = do
    xprs <- parseForth str
    return $ compileExprs xprs

testComp str = putStrLn $ let Right x = parseForth str in A.printTree $ compileExprs x
