module Annotate (annotateProg) where

import AST
import AST2
import Division
import Values

annotateProg :: Ord a => DivisionPW a -> NormProgram a -> Program' a
annotateProg d (_, p)= map (annotateBlock d) p

annotateBlock :: Ord a => DivisionPW a -> NormBlock a -> Block' a
annotateBlock pwd b = 
  Block' 
    { name' = nname b
    , initDiv = d1
    , from' = annotateFrom d1 $ nfrom b
    , body' = [annotateStep d1 d2 $ nstep b]
    , jump' = annotateGoto d2 $ njump b
    , endDiv = d2
    } 
  where 
    (d1, d2) = getDivs (nname b) pwd
    

annotateStep :: Division -> Division -> Step -> Step'
annotateStep d1 _ (Update n rop e) = 
  case getType n d1 of
    BTStatic -> 
      case annotateExp d1 e of
        (e', BTStatic) -> Update' BTStatic n rop e'
        _ -> undefined
    BTDynamic -> 
      Update' BTDynamic n rop $ lift (annotateExp d1 e)
annotateStep d1 d2 (Replacement q1 q2) = 
  let (q1', btType1) = annotatePat d2 q1
      (q2', btType2) = annotatePat d1 q2
      btType = btType1 `lub` btType2
  in Replacement' btType q1' q2'
annotateStep d1 _ (Assert e) = 
  let (e', btType) = annotateExp d1 e 
  in Assert' btType e'
annotateStep _ _ Skip = Skip' BTStatic               

annotateFrom :: Division -> ComeFrom a () -> ComeFrom' a
annotateFrom _ (Entry ()) = Entry'
annotateFrom _ (From (l, ())) = From' l
annotateFrom d (Fi e (l1, ()) (l2, ())) =
  let (e', btType) = annotateExp d e 
  in Fi' btType e' l1 l2

annotateGoto :: Division -> Jump a () -> Jump' a
annotateGoto _ (Exit ()) = Exit'
annotateGoto _ (Goto (l, ())) = Goto' l
annotateGoto d (If e (l1, ()) (l2, ())) =
  let (e', btType) = annotateExp d e 
  in If' btType e' l1 l2

annotatePat :: Division -> Pattern -> (Pattern', Level)
annotatePat _ (QConst v) = (QConst' BTStatic v, BTStatic)
annotatePat d (QVar n) = 
  let btType = getType n d 
  in (QVar' btType n, btType)  
annotatePat d (QPair q1 q2) = 
  let (q1', t1) = annotatePat d q1
      (q2', t2) = annotatePat d q2 
  in case t1 `lub` t2 of
    BTStatic -> (QPair' BTStatic q1' q2', BTStatic)
    BTDynamic -> (QPair' BTDynamic (liftQ (q1',t1)) (liftQ (q2',t2)), BTDynamic)

annotateExp :: Division -> Expr -> (Expr', Level)
annotateExp _ (Const i) = (Const' BTStatic i, BTStatic)
annotateExp d (Var n) = 
  let btType = getType n d 
  in (Var' btType n, btType)   
annotateExp d (Op bop e1 e2) = 
  let (e1', t1) = annotateExp d e1
      (e2', t2) = annotateExp d e2 
  in case t1 `lub` t2 of
    BTStatic -> (Op' BTStatic bop e1' e2', BTStatic)
    BTDynamic -> (Op' BTDynamic bop (lift (e1',t1)) (lift (e2',t2)), BTDynamic)
annotateExp d (UOp op e) = 
  let (e', btType) = annotateExp d e 
  in (UOp' btType op e', btType)

liftQ :: (Pattern', Level) -> Pattern'
liftQ (q, BTStatic) = QLift q
liftQ (q, BTDynamic) = q

lift :: (Expr', Level) -> Expr'
lift (e, BTStatic) = Lift e
lift (e, BTDynamic) = e
