{-# LANGUAGE QuasiQuotes, PackageImports #-}
module FORTH.Compiler (compileProg, compileText, compileProgWithIncs, compileTextWithIncs) where

import FORTH.Parser
import FORTH.Stdlib
import Asm.QQ
import Data.Maybe
import Data.List
import Data.List.Utils
import Control.Applicative
import Data.Word
import qualified Asm.Expr as A
import qualified Asm.Parser as A
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as B

import "mtl" Control.Monad.State.Lazy

type Strings = Map.Map String String
type Vars    = [String]
type Ifs     = [String]

data RtnWord = RtnWord String [A.Expr]

data FState = FState { strs :: Strings
                     , vars :: Vars
                     , ifs :: Ifs
                     , ctrlGen :: LabelGen
                     , wordLbls :: Map.Map String String
                     }

-- Pushes an IF onto the 'ifs' stack, and returns the IF label as well as the new state
pushIf :: State FState String
pushIf = do
    newIf <- newLabel
    state <- get
    put state { ifs = newIf:ifs state }
    return newIf

popIf :: State FState (Maybe String)
popIf = do
    state <- get
    case ifs state of
        [] -> return Nothing
        xs -> do
            put state { ifs = tail xs }
            return . Just . head $ xs

callWord :: String -> State FState [A.Expr]
callWord t = do
    lbl <- wordLabel t
    return [asm|call @{lbl}|]

wordLabel :: String -> State FState String
wordLabel nm = do
    lbl <- Map.lookup nm . wordLbls <$> get
    case lbl of
        Just x -> return x
        Nothing -> do
            x <- newLabel
            s <- get
            put s { wordLbls = Map.insert nm x (wordLbls s) }
            return x

newLabel :: State FState String
newLabel = do
    state <- get
    let (lbl, gen) = genLabel (ctrlGen state)
    put state { ctrlGen = gen }
    return lbl

data LabelGen = LabelGen String Int

makeLabelGen :: String -> LabelGen
makeLabelGen prefix = LabelGen prefix 0

genLabel :: LabelGen -> (String, LabelGen)
genLabel (LabelGen p c) = (p ++ "_" ++ show c, LabelGen p (c + 1))

wordPrologue nm = [asm|
    @{nm ++ ":"}
    pop hl
    dec ix
    ld (ix),h
    dec ix
    ld (ix),l
|]

wordEpilogue = [asm|
    ld l,(ix)
    inc ix
    ld h,(ix)
    inc ix
    jp (hl)
|]

defWordFull :: String -> [A.Expr] -> State FState [A.Expr]
defWordFull nm code = do
    lbl <- wordLabel nm
    return $ wordPrologue lbl ++ code ++ wordEpilogue

defWord :: Expr -> State FState [A.Expr]
defWord (WordDef nm body) = do
    lbl <- wordLabel nm
    code <- concat <$> mapM (compileBody lbl) body
    let allCode = if rwDef lbl `notElem` code
                      then rwDef lbl:code
                      else code
    defWordFull nm allCode
    where compileBody lbl x = case x of
              Tok "RECURSE" -> return [asm|jp @{"RWDEF_" ++ lbl}|]
              Tok "IFRECURSE" -> return [asm|
                      pop hl
                      ld a,h
                      or l
                      jp nz,@{"RWDEF_" ++ lbl}
                  |]
              Tok "RETURN" -> let Just rtn = stdlibAsm "RETURN" in return $ code rtn
              Tok "RECURSEPOINT" -> return [rwDef lbl]
              _ -> compileExpr x
          rwDef lbl = A.LabelDef ("RWDEF_" ++ lbl)

defWordAsm :: Expr -> State FState [A.Expr]
defWordAsm (WordDefAsm nm body) = defWordFull nm code
    where code = case A.parseText "" body of
              Left err -> error err
              Right xs -> xs

defRtnWord :: RtnWord -> State FState [A.Expr]
-- Stdlib routines handle returning themself, so we shouldnt wrap them in the prologue/epilogue
defRtnWord (RtnWord nm body) = do
    lbl <- wordLabel nm
    return $ [asm|@{lbl ++ ":"}|] ++ body


defString :: (String, String) -> [A.Expr]
defString (s,lbl) = [asm|@{lbl ++ ":"} \ .db @s{s},0|]

defVar :: String -> [A.Expr]
defVar v = [asm|.var word, @{"VAR_" ++ v}|]

-- Compile an expression into the resulting ASM code
compileExpr :: Expr -> State FState [A.Expr]
compileExpr (Num x) = return [asm|ld hl,@{x} \ push hl|]
compileExpr (Str s) = do
    Just lbl <- Map.lookup s . strs <$> get
    return [asm|ld hl,@{lbl} \ push hl|]

compileExpr (Tok "IF") = do
    lbl <- pushIf
    return [asm|
        pop hl
        ld a,h
        or l
        jp z,@{lbl}
    |]

compileExpr (Tok "ELSE") = do
    lbl <- popIf
    case lbl of
        Nothing -> error "No matching 'IF' for 'ELSE' word"
        Just x  -> do
            let jp = "EL" ++ x
            state <- get
            put state { ifs = jp:ifs state }
            -- Jump to the end label skipping the else statement
            return [asm|jp @{jp} \ @{x ++ ":"}|]

compileExpr (Tok "THEN") = do
    lbl <- popIf
    case lbl of
        Nothing  -> error "No matching 'IF' or 'ELSE' for 'THEN' word"
        Just x   -> return [asm|@{x ++ ":"}|]

compileExpr (Tok "RECURSE") = error "Can not use 'RECURSE' outside of word def"

compileExpr (Tok "RETURN") = error "Can not use 'RETURN' outside of word def"

compileExpr (Tok t)
    | isNothing rtn = do
        state <- get
        case t `elem` vars state of
            True -> return [asm|ld hl,@{"VAR_" ++ t} \ push hl|]
            False -> callWord t
    | otherwise = do
        let Just x = rtn
        case inline x of
            True -> do
                -- Make sure inline stuff gets added to used words
                wordLabel t
                return $ code x
            False -> callWord t
    where rtn = stdlibAsm t

compileExpr _ = return []


-- Various ASM start/end code for different calc models
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

-- Optimizations that apply to most of the code.
optimizeAsm :: [A.Expr] -> [A.Expr]
optimizeAsm = multiple (pass1 . pass2 . pass3)
    where pass1 = replace [asm|push hl \ pop hl|] []
          pass2 = replace [asm|push hl \ pop de|] [asm|ld d,h \ ld e,l|]
          pass3 = replace [asm|push de \ pop hl|] [asm|ld h,d \ ld l,e|]
          multiple f = f . f . f -- Really really optimize it

-- Gets all defined strings and maps them to labels
getStrings :: [Expr] -> Strings
getStrings xprs
    | null strs = Map.empty
    | otherwise = Map.fromList [strPair x | x <- [0..length strs - 1]]
    where strs = findStrs xprs
          strPair x = (strs !! x, "STRCNST_" ++ show x)
          findStrs xs = [x | (Str x) <- xs] ++ concat [findStrs x | (WordDef _ x) <- xs]

-- Gets all defined vars
getVars :: [Expr] -> Vars
getVars xprs = [x | (VarDef x) <- xprs] ++ concat [getVars x | (WordDef _ x) <- xprs]

-- Compiles a FORTH program into Assembly
-- Pass [] for startVars if you have no pre-defined variables
compileProg :: [Expr] -> Vars -> [A.Expr]
compileProg xprs startVars =
    let (body, wordDefs) = flip evalState initState $ do
        -- Get all word definitions
        let asmWords  = [w | w@(WordDefAsm nm _) <- xprs]
            normWords = [w | w@(WordDef nm _)    <- xprs]

        -- Compile all expressions, generating word labels in the process
        asmDefs  <- mapM defWordAsm asmWords
        normDefs <- mapM defWord normWords
        body <- mapM compileExpr xprs

        -- Get the STDLIB word definitions actually called in the program
        usedWords <- wordLbls <$> get
        let usedRtns = [(t,rtn) | t <- Map.keys usedWords,
                           let rtn' = stdlibAsm t,
                           isJust rtn',
                           let Just rtn = rtn']
            rtnWords = [RtnWord t (code rtn) | (t,rtn) <- usedRtns, not (inline rtn)]
            rtnDeps  = concatMap (depends . snd) usedRtns
        rtnDefs <- mapM defRtnWord rtnWords

        -- Return the body of the code, and all the word definitions
        return (body, normDefs ++ rtnDefs ++ asmDefs ++ rtnDeps)
    in optimizeAsm $ concatMap concat [
        map defVar vars,
        [asmStart platform],
        body,
        [asmEnd platform],
        wordDefs,
        map defString (Map.toList strings)
    ]
    where strings = getStrings xprs
          vars = getVars xprs ++ startVars
          platform = "ti84pcse"
          initState = FState { strs = strings
                             , vars = vars
                             , ifs = []
                             , ctrlGen = makeLabelGen "CTRL"
                             , wordLbls = Map.empty }

binaryInc :: String -> String -> IO [A.Expr]
binaryInc nm fname = do
    contents <- B.readFile fname
    let bytes = B.unpack contents
    return $ [asm|
        @{"VAR_" ++ nm ++ ":"}
        @{".db " ++ intercalate "," (map show bytes)}
    |]

compileProgWithIncs :: [Expr] -> IO [A.Expr]
compileProgWithIncs xprs = do
    let incs = [(nm, binaryInc nm fname) | BinaryInc nm fname <- xprs]
        binvars = map fst incs
    bytes <- concat <$> mapM snd incs
    return $ compileProg xprs binvars ++ bytes

compileText :: String -> Either String [A.Expr]
compileText str = do
    xprs <- parseForth str
    return $ compileProg xprs []

compileTextWithIncs :: String -> IO (Either String [A.Expr])
compileTextWithIncs str = do
    let xprs = parseForth str
    case xprs of
      Left err -> return (Left err)
      Right code -> do
          asm <- compileProgWithIncs code
          return (Right asm)
