{-# LANGUAGE FlexibleInstances #-}

module RL.Valueable (valuealizeProg, Valueable (..)) where

{- Conversion from RL programs to RL values. -}

import RL.AST
import RL.Values

valuealizeProg :: Program String store -> Value
valuealizeProg (decl, blocks) = asValue (decl, mapLabel Atom blocks)

-- | Class of things that can be converted to RL values
class Valueable a where
  asValue :: a -> Value

instance (Valueable a) => Valueable [a] where
  asValue = foldr (Pair . asValue) Nil

instance (Valueable a, Valueable b) => Valueable (a, b) where
  asValue (a, b) = Pair (asValue a) (asValue b)

instance (Valueable a, Valueable b, Valueable c) => Valueable (a, b, c) where
  asValue (a, b, c) = Pair (asValue a) (Pair (asValue b) (asValue c))

instance (Valueable a, Valueable b, Valueable c, Valueable d) => Valueable (a, b, c, d) where
  asValue (a, b, c, d) = Pair (asValue a) (Pair (asValue b) (Pair (asValue c) (asValue d)))

instance Valueable Value where
  asValue = id

instance Valueable UnOp where
  asValue Hd = Atom "HD"
  asValue Tl = Atom "TL"
  asValue Not = Atom "NOT"

instance Valueable RevOp where
  asValue Add = Atom "ADD"
  asValue Sub = Atom "SUB"
  asValue Xor = Atom "XOR"

instance Valueable BinOp where
  asValue (ROp revop) = asValue revop
  asValue Mul = Atom "MUL"
  asValue Div = Atom "DIV"
  asValue And = Atom "AND"
  asValue Or = Atom "OR"
  asValue Less = Atom "LESS"
  asValue Greater = Atom "GREATER"
  asValue Equal = Atom "EQUAL"
  asValue Cons = Atom "CONS"

instance Valueable Pattern where
  asValue (QConst v) = asValue (Atom "CONST", v)
  asValue (QVar n) = asValue (Atom "VAR", Atom n)
  asValue (QPair p1 p2) = asValue (Atom "PAIR", p1, p2)

instance Valueable Expr where
  asValue (Const v) = asValue (Atom "CONST", v)
  asValue (Var n) = asValue (Atom "VAR", Atom n)
  asValue (Op op e1 e2) = asValue (Atom "OP", op, e1, e2)
  asValue (UOp op e) = asValue (Atom "UOP", op, e)

instance Valueable Step where
  asValue Skip = Atom "SKIP"
  asValue (Assert e) = asValue (Atom "ASSERT", e)
  asValue (Replacement p1 p2) = asValue (Atom "REPLACE", p1, p2)
  asValue (Update n o e) = asValue (Atom "UPDATE", o, Atom n, e)

instance (Valueable label) => Valueable (Jump label store) where
  asValue (Exit _) = Atom "EXIT"
  asValue (Goto (l, _)) = asValue (Atom "GOTO", asValue l)
  asValue (If e (l1, _) (l2, _)) = asValue (Atom "IF", e, asValue l1, asValue l2)

instance (Valueable label) => Valueable (ComeFrom label store) where
  asValue (Entry _) = Atom "ENTRY"
  asValue (From (l, _)) = asValue (Atom "FROM", asValue l)
  asValue (Fi e (l1, _) (l2, _)) = asValue (Atom "FI", e, asValue l1, asValue l2)

instance (Valueable label) => Valueable (Block label store) where
  asValue (Block (n, _) f b j) = asValue (asValue n, f, b, j)

instance Valueable VariableDecl where
  asValue (VariableDecl i o t) = asValue (Atom <$> i, Atom <$> o, Atom <$> t)
