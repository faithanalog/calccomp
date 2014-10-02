module Asm.Expr where

type Ident = String
type Instr = String
type Reg = String

data Expr = Label String
          | Instr String [Expr]
          | Directive String [Expr]
          | Reg String
          | Num Integer
          | String String
          deriving (Eq,Show)
