module AST where

import Values 

type Program label = [Block label]

data Block label = Block 
  { name :: label
  , from :: IfFrom label
  , body :: [Step]
  , jump :: Jump label
  }
  deriving (Eq, Show, Read)


-- TODO: Better name, Landing/Origin?
data IfFrom label = 
    From label 
  | FromCond Expr label label 
  | Entry
  deriving (Eq, Show, Read)

data Jump label = 
    Goto label 
  | If Expr label label 
  | Exit
  deriving (Eq, Show, Read)

data Step = 
    Update Name RevOp Expr
  | Replacement Pattern Pattern
  | Assert Expr
  | Skip
  deriving (Eq, Show, Read)

data Expr =
    Const Value
  | Var Name
  | Op BinOp Expr Expr
  | UOp UnOp Expr
  deriving (Eq, Show, Read)

data Pattern =
    QConst Value
  | QVar Name
  | QPair Pattern Pattern
  deriving (Eq, Show, Read)

data BinOp =
    ROp RevOp
  | Mul
  | Div
  | And
  | Or
  | Less
  | Greater
  | Equal
  | Index
  | Cons
  deriving (Eq, Show, Read)

data RevOp =
    Add
  | Sub
  | Xor
  deriving (Eq, Show, Read)

data UnOp = 
    Hd
  | Tl
  | Not
  deriving (Eq, Show, Read)

type Program' label = [Block' label]

data Block' label = Block' 
  { name' :: label
  , from' :: IfFrom' label
  , body' :: [Step']
  , jump' :: Jump' label
  }
  deriving (Eq, Show, Read)

data IfFrom' label = 
    From' label
  | FromCond' Level Expr' label label 
  | Entry'
  deriving (Eq, Show, Read)

data Jump' label = 
    Goto' label 
  | If' Level Expr' label label 
  | Exit'
  deriving (Eq, Show, Read)

data Step' = 
    Update' Level Name RevOp Expr'
  | Replacement' Level Pattern Pattern
  | Assert' Level Expr'
  | Skip' Level
  deriving (Eq, Show, Read)

data Expr' =
    Const' Level Value
  | Var' Level Name
  | Op' Level BinOp Expr' Expr'
  | UOp' Level UnOp Expr'
  | Lift Expr'
  deriving (Eq, Show, Read)