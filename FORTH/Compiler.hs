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

defWord :: Strings -> Vars -> Expr -> [A.Expr]
defWord strs vars (WordDef nm prebody) = prologue ++ code ++ epilogue
    where code = concatMap (compileExpr strs vars) body
          body = Tok ">R" : prebody
          prologue = [asm|@{"WDEF_" ++ nm ++ ":"}|]
          epilogue = [asm|
              ld l,(ix)
              inc ix
              ld h,(ix)
              inc ix
              jp (hl)
          |]

defWordAsm :: Expr -> [A.Expr]
defWordAsm (WordDefAsm nm body) = prologue ++ code ++ epilogue
    where code = case A.parseText "" body of
              Left err -> error err
              Right xs -> xs
          prologue = [asm|
              @{"WDEF_" ++ nm ++ ":"}
              pop hl
              dec ix
              ld (ix),h
              dec ix
              ld (ix),l
          |]
          epilogue = [asm|
              ld l,(ix)
              inc ix
              ld h,(ix)
              inc ix
              jp (hl)
          |]

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
          code = [prologue] ++ map (compileExpr strings vars) xprs ++ [epilogue]
          prologue = [asm|
              CODE_START:
              .org UserMem - 2
              .db tExtTok, tAsm84CCmp
              .varloc statVars, 531
              .var word, oldStack
              ld (oldStack),sp
              ld ix,saveSScreen + 300h
          |]
          epilogue = [asm|
              ld sp,(oldStack)
              b_call(_DelRes)
              ret
              CODE_END:
          |]

compileText :: String -> Either String [A.Expr]
compileText str = do
    xprs <- parseForth str
    return $ compileExprs xprs

testComp str = putStrLn $ let Right x = parseForth str in A.printTree $ compileExprs x
