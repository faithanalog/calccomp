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
import qualified Data.Set as Set
import Control.Monad.State.Lazy

type Strings = Map.Map String String
type Vars    = [String]
type Ifs     = [String]

data FState = FState { strs :: Strings
                     , vars :: Vars
                     , ifs :: Ifs
                     , ifGen :: LabelGen
                     , usedWords :: Set.Set WordDefAsm
                     , curWord :: String
                     }

-- Pushes an IF onto the 'ifs' stack, and returns the IF label as well as the new state
pushIf :: State FState String
pushIf = do
    state <- get
    let (newIf, gen) = genLabel (ifGen state)
    put state { ifs = newIf:ifs state, ifGen = gen }
    return newIf

-- pushIf :: State -> (String, State)
-- pushIf state = (newIf, state { ifs = (newIf:ifs state), ifGen = gen })
--     where (newIf, gen) = genLabel (ifGen state)

-- Pushes an 'ELSE' statement onto the 'ifs' stack, so the THEN word will
-- know we already popped the IF label
pushElse :: State FState ()
pushElse = do
    state <- get
    put state { ifs = "|ELSE|":ifs state }

popIf :: State FState (Maybe String)
popIf = do
    state <- get
    case ifs state of
        [] -> return Nothing
        xs -> do
            put state { ifs = tail xs }
            return . Just . head $ xs

-- popIf :: State -> (Maybe String, State)
-- popIf state
--     | null (ifs state) = (Nothing, state)
--     | otherwise = let (x:xs) = ifs state in (Just x, state { ifs = xs })

data LabelGen = LabelGen String Int

makeLabelGen :: String -> LabelGen
makeLabelGen prefix = LabelGen prefix 0

genLabel :: LabelGen -> (String, LabelGen)
genLabel (LabelGen p c) = (p ++ "_" ++ show c, LabelGen p (c + 1))

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

defWord :: Expr -> State FState [A.Expr]
defWord = do


-- Not done, rewrite body
defWord :: State -> Expr -> (State, [A.Expr])
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

compileExpr :: State -> Expr -> ([A.Expr], State)
compileExpr state (Num x) = ([asm|ld hl,@{x} \ push hl|], state)
compileExpr state (Str s) =
    let Just lbl = Map.lookup s (strs state)
    in ([asm|ld hl,@{lbl} \ push hl|], state)

compileExpr state (Tok "IF") = ([asm|
    pop hl
    ld a,h
    or l
    jp z,@{lbl}
|], nstate)
    where (lbl, nstate) = pushIf state

compileExpr state (Tok "ELSE")
    | isNothing lbl = error "No matching 'IF' for 'ELSE' word"
    | otherwise = ([asm|
        @{lbl ++ ":"}
    |], pushElse nstate)
    where (lbl, nstate) = popIf state

compileExpr state (Tok "THEN")
    | isNothing lbl = error "No matching 'IF' for 'THEN' word"
    | lbl == "|ELSE|" = ([],nstate)
    | otherwise = ([asm|
        @{lbl ++ ":"}
    |], nstate)
    where (lbl, nstate) = popIf state

compileExpr state (Tok "RECURSE")
    | null (curWord state) = error "Can not use 'RECURSE' outside of word def"
    | otherwise = ([asm|jp @{"RWDEF_" ++ curWord state}|], state)

compileExpr state (Tok t)
    | isNothing rtn = (if t `elem` vars state then ldVar else callWord, state)
    | otherwise = (if inline x then code x else callWord,
        state { usedWords = Set.insert rtnWordDef (usedWords state) })
    where rtn = stdlibAsm t
          rtnWordDef = let x = Just rtn in WordDefAsm t (code x)
          callWord = [asm|call @{"WDEF_" ++ t}|]
          ldVar    = [asm|ld hl,@{"VAR_" ++ t} \ push hl|]

compileExpr state _ = ([], state)

compileExprs :: State -> [Expr] -> ([A.Expr],State)
compileExprs state xs = foldl ([],state) folder xs
    where folder (c,s) xpr =
              let (nc,ns) = compileExpr s xpr
              in  (c ++ nc, ns)

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

compileProg :: [Expr] -> [A.Expr]
compileProg xprs = concatMap concat [
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
