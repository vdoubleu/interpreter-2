import Data.Function
import Data.List

type INTVAR = String
type BOOLVAR = String
type VARNAME = String
data MATHOP
  = MATHOP :+: MATHOP
  | MATHOP :-: MATHOP
  | MATHOP :*: MATHOP
  | MATHOP :/: MATHOP
  | I Int
  | IV INTVAR
data BINOP
  = BINOP :&&: BINOP
  | BINOP :||: BINOP
  | EXPR :==: EXPR
  | B Bool
data EXPR = MO MATHOP | BO BINOP
data STMT
  = E EXPR
  | VARNAME :=: EXPR
  | While BINOP STMTS
type STMTS = [STMT]
type Store = [(String, Int)]

interpSts :: STMTS -> Store -> (Store, Int)
interpSts [] s = error "expected statements to interp, but none were found"
interpSts (p:ps) s =
  let (headStore, headRes) = (interpStmt p s)
  in (if null ps
      then (headStore, headRes)
    else interpSts ps headStore)

interpStmt :: STMT -> Store -> (Store, Int)
interpStmt (E expr) store = interpExpr expr store
interpStmt (varname :=: expr) store = 
  let (exprStore, exprRes) = interpExpr expr store
      newStore = (varname, exprRes):exprStore
  in (newStore, exprRes)
interpStmt (While b stmts) store = interpWhile b stmts store

interpWhile :: BINOP -> STMTS -> Store -> (Store, Int)
interpWhile cond stmts store =
  let (condStore, condRes) = interpBinOp cond store
  in (if condRes
      then (let (stsStore, stsRes) = interpSts stmts condStore
        in interpWhile cond stmts stsStore)
      else (condStore, 0))

interpExpr :: EXPR -> Store -> (Store, Int)
interpExpr (MO mathop) store = interpMathOp mathop store

interpBinOp :: BINOP -> Store -> (Store, Bool)
interpBinOp (b1 :&&: b2) store = 
  let (b1Store, b1Res) = interpBinOp b1 store
      (b2Store, b2Res) = interpBinOp b2 b1Store
  in (b2Store, (b1Res && b2Res))
interpBinOp (b1 :||: b2) store = 
  let (b1Store, b1Res) = interpBinOp b1 store
      (b2Store, b2Res) = interpBinOp b2 b1Store
  in (b2Store, (b1Res || b2Res))
interpBinOp (e1 :==: e2) store =
  let (e1Store, e1Res) = interpExpr e1 store
      (e2Store, e2Res) = interpExpr e2 e1Store
  in (e2Store, (e1Res == e2Res))
interpBinOp (B boolconst) store = (store, boolconst)

interpMathOp :: MATHOP -> Store -> (Store, Int)
interpMathOp (m1 :+: m2) store =
  let (m1Store, m1Res) = interpMathOp m1 store
      (m2Store, m2Res) = interpMathOp m2 m1Store
  in (m2Store, m1Res + m2Res)
interpMathOp (m1 :-: m2) store = 
  let (m1Store, m1Res) = interpMathOp m1 store
      (m2Store, m2Res) = interpMathOp m2 m1Store
  in (m2Store, m1Res - m2Res)
interpMathOp (m1 :*: m2) store = 
  let (m1Store, m1Res) = interpMathOp m1 store
      (m2Store, m2Res) = interpMathOp m2 m1Store
  in (m2Store, m1Res * m2Res)
interpMathOp (m1 :/: m2) store = 
  let (m1Store, m1Res) = interpMathOp m1 store
      (m2Store, m2Res) = interpMathOp m2 m1Store
  in (m2Store, (div m1Res m2Res))
interpMathOp (I intconst) store = (store, intconst)
interpMathOp (IV intvar) store = case (lookup intvar store) of
  Nothing -> error "unbound variable"
  Just intRes -> (store, intRes)

-}
