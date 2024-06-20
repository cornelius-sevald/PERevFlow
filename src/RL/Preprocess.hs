module RL.Preprocess (preprocessProgram, encodeProgram, encodeStore) where

import Control.Arrow
import qualified Control.Monad.State as State
import RL.AST
import RL.Valueable
import RL.Values
import Utils.Maps

{- Preprocessing of RL programs so that they can be
 - interpreted by a self-interpreter. -}

encodeStore :: Store -> Value
encodeStore = asValue . map (first Atom) . toList

encodeProgram :: Program String store -> Value
encodeProgram (_, blocks) = asValue $ mapLabel Atom blocks

preprocessProgram :: (Eq label, Eq store) => Program label store -> Maybe (Program label store)
preprocessProgram (decl, blocks) = do
  blocks' <- reorderBlocks blocks
  let prepBlocks = do tmp <- freshName; return $ preprocessBlock tmp <$> blocks'
      (prepBlocks', tmpVars) = runNameSource decl prepBlocks
  let decl' = VariableDecl {input = input decl, output = output decl, temp = temp decl ++ tmpVars}
  return (decl', prepBlocks')

reorderBlocks :: (Eq label, Eq store) => [Block label store] -> Maybe [Block label store]
reorderBlocks blocks = do
  [entryBlock] <- pure $ filter isEntry blocks
  [exitBlock] <- pure $ filter isExit blocks
  let otherBlocks = filter (\b -> not (isEntry b || isExit b)) blocks
  let blocks' =
        if entryBlock == exitBlock
          then [entryBlock]
          else [entryBlock] ++ otherBlocks ++ [exitBlock]
  return blocks'

preprocessBlock :: Name -> Block l s -> Block l s
preprocessBlock tmpName (Block {name = _name, jump = _jump, from = _from, body = _body}) =
  let _body' = concat $ map (removeReplace tmpName) _body
   in Block {name = _name, jump = _jump, from = _from, body = _body'}

-- | Replace replace steps with non-replace steps
removeReplace :: Name -> Step -> [Step]
removeReplace tmpName (Replacement q1 q2) =
  -- Step for initializing temporary var
  let initTmp = Update tmpName Xor (patToExpr q2)
      -- Steps for clearing variables in q2n q2n q2n q2n q2n q2
      clearQ2 = map clear $ unfoldPat (Var tmpName) q2
      -- Steps for assigning variables in q1. q1. q1. q1. q1. q1.
      assignQ1 = map assign $ unfoldPat (Var tmpName) q1
      -- Step for clearing temporary var var var var var var
      clearTmp = Update tmpName Xor (patToExpr q1)
   in [initTmp] ++ clearQ2 ++ assignQ1 ++ [clearTmp]
  where
    clear (Left _, _) = Skip
    clear (Right x, e) = Update x Xor e
    assign (Left v, e) = Assert (Op Equal (Const v) e)
    assign (Right x, e) = Update x Xor e
removeReplace _ s = [s]

-- Convert a pattern to an equivalent expression
patToExpr :: Pattern -> Expr
patToExpr (QConst v) = Const v
patToExpr (QVar x) = Var x
patToExpr (QPair q1 q2) = Op Cons e1 e2
  where
    e1 = patToExpr q1
    e2 = patToExpr q2

-- Unfold a pattern into a list of expressions,
-- such that the expressions "find" the constant/variable in the pattern.
-- Need to provide a base expression, which should evaluate to a value with
-- the same shape as the pattern.
unfoldPat :: Expr -> Pattern -> [(Either Value Name, Expr)]
unfoldPat e (QConst v) = pure (Left v, e)
unfoldPat e (QVar x) = pure (Right x, e)
unfoldPat e (QPair q1 q2) =
  let l1 = unfoldPat (UOp Hd e) q1
      l2 = unfoldPat (UOp Tl e) q2
   in l1 ++ l2

-- | A source of fresh names.
--   Also keeps track of all used names
type NameSource a = State.State ([Name], [Name]) a

-- Run a computation in an environment with a fresh source of names.
-- Gives output of computation as well as any fresh names used.
runNameSource :: VariableDecl -> NameSource a -> (a, [Name])
runNameSource (VariableDecl {temp = _temp, output = _output, input = _input}) ns =
  second fst $ State.runState ns ([], fresh)
  where
    -- Variables already declared
    declared = _temp ++ _output ++ _input
    -- Infinite list of variable names
    names = (\i -> "TMP__" ++ show i) <$> [0 :: Integer ..]
    -- Infinite list of not-already-declared variable names
    fresh = filter (not . flip elem declared) names

freshName :: NameSource Name
freshName = do
  (used, source) <- State.get
  let fresh = head source
  let source' = tail source
  State.put (fresh : used, source')
  return fresh
