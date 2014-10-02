module C.Expr where

type Id = String

type FuncArgs = [Expr]

type FuncBody = [Expr]

data DataType = Void | Uint8 | Uint16 deriving (Eq,Show)

data Op = Add | Sub | Mul | Div | Assign deriving (Eq,Show)

data Expr = FuncDef Id DataType FuncArgs FuncBody
          | VarDef Id DataType
          | FuncCall Id FuncArgs
          | Var Id
          | Num Integer
          | String String
          | Binop Op Expr Expr
          | Asm String
          deriving (Eq,Show)

addExpr :: Expr -> Expr -> Expr
addExpr = Binop Add

subExpr :: Expr -> Expr -> Expr
subExpr = Binop Sub

mulExpr :: Expr -> Expr -> Expr
mulExpr = Binop Mul

divExpr :: Expr -> Expr -> Expr
divExpr = Binop Div

assignExpr :: Expr -> Expr -> Expr
assignExpr = Binop Assign


sizeOf :: DataType -> Int
sizeOf Void = 0
sizeOf Uint8 = 1
sizeOf Uint16 = 2