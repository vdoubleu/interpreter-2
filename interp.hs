import Data.Function
import Data.List


data TypeVal
  = TVI Int
  | TVB Bool 
  | TVF FUNC

instance Show TypeVal
  where show (TVI i) = "(TVI " ++ (show i) ++ ")"
        show (TVB b) = "(TVB " ++ (show b) ++ ")"

-- env
type Store = [(String, TypeVal)]

data MATHOP
  = MATHOP :+: MATHOP
  | MATHOP :-: MATHOP
  | I Int
  | IV String

data BINOP
  = BINOP :&&: BINOP
  | BINOP :||: BINOP
  | EXPR :==: EXPR
  | B Bool
  | BV String

data EXPR
  = MO MATHOP
  | BO BINOP

data STMT
  = E EXPR
  | String :=: EXPR  -- a variable can only store a bool or an int, no functions right now
  | While EXPR STMTS
  | If EXPR STMTS STMTS
  | Print EXPR

type STMTS = [STMT]

data ARG = A String TypeVal
type ARGS = [ARG]
data FUNC = F String ARGS STMTS

data PROGRAM = PGSTS STMT PROGRAM | PGFNS FUNC PROGRAM | EOP

interp :: PROGRAM -> Store -> (Int, Store)
interp (PGSTS stmt program) store = 
  let (stmtRes, stmtStore) = interpStmt stmt store
  in  interp program stmtStore
interp (PGFNS func program) store = 
  let (F name args stmts) = func
  in  interp program ((name, (TVF func)):store)
interp EOP store = (0, store)

interpStmts :: STMTS -> Store -> (TypeVal, Store)
interpStmts [] store = ((TVI 0), store)
interpStmts (stmt:[]) store = interpStmt stmt store
interpStmts (stmt:stmts) store =
  let (stmtRes, stmtStore) = interpStmt stmt store
  in  interpStmts stmts stmtStore

interpStmt :: STMT -> Store -> (TypeVal, Store)
interpStmt (E expr) store = interpExpr expr store
interpStmt (name :=: expr) store = 
  let (exprRes, exprStore) = interpExpr expr store
  in  (exprRes, (name, exprRes):exprStore)
interpStmt (While expr stmts) store = interpWhile expr stmts store
interpStmt (If testExpr trueCase falseCase) store = 
  case (interpExpr testExpr store) of
    ((TVI i), testExprStore) ->
      (if (intAsBool i)
        then (interpStmts trueCase testExprStore)
        else (interpStmts falseCase testExprStore))
    ((TVB b), testExprStore) -> 
      (if b
        then (interpStmts trueCase testExprStore)
        else (interpStmts falseCase testExprStore))
--interpSts (Print expr) store = 

interpWhile :: EXPR -> STMTS -> Store -> (TypeVal, Store)
interpWhile expr stmts store = 
  case (interpExpr expr store) of
    ((TVI i), exprStore) -> 
      (if (intAsBool i)
        then
          (let (stmtRes, stmtStore) = (interpStmts stmts store)
          in   interpWhile expr stmts stmtStore)
        else((TVI 0), exprStore))
    ((TVB b), exprStore) -> 
      (if b 
      then 
        (let (stmtRes, stmtStore) = (interpStmts stmts store) 
        in  interpWhile expr stmts stmtStore)
      else ((TVI 0), exprStore))

interpExpr :: EXPR -> Store -> (TypeVal, Store)
interpExpr (MO mathop) store = interpMathOp mathop store
interpExpr (BO binop) store = interpBinOp binop store

interpBinOp :: BINOP -> Store -> (TypeVal, Store)
interpBinOp (b1 :&&: b2) store =
  let ((TVB b1Res), b1Store) = interpBinOp b1 store
      ((TVB b2Res), b2Store) = interpBinOp b2 b1Store
  in  (TVB (b1Res && b2Res), b2Store)
interpBinOp (b1 :||: b2) store =
  let ((TVB b1Res), b1Store) = interpBinOp b1 store
      ((TVB b2Res), b2Store) = interpBinOp b2 b1Store
  in  (TVB (b1Res || b2Res), b2Store)
interpBinOp (e1 :==: e2) store =
  let (e1Res, e1Store) = interpExpr e1 store
      (e2Res, e2Store) = interpExpr e2 e1Store
  in  (TVB (typeValEq e1Res e2Res), e2Store)
interpBinOp (B boolConst) store = (TVB boolConst, store)
interpBinOp (BV boolVar) store = 
  case (lookup boolVar store) of
    Nothing -> error ("variable: " ++ boolVar ++ " not bound")
    Just (TVB b) -> (TVB b, store)
    Just t -> error ("in variable lookup found something but expected boolean")

interpMathOp :: MATHOP -> Store -> (TypeVal, Store)
interpMathOp (m1 :+: m2) store =
  let ((TVI m1Res), m1Store) = interpMathOp m1 store
      ((TVI m2Res), m2Store) = interpMathOp m2 m1Store
  in  (TVI (m1Res + m2Res), m2Store)
interpMathOp (m1 :-: m2) store =
  let ((TVI m1Res), m1Store) = interpMathOp m1 store
      ((TVI m2Res), m2Store) = interpMathOp m2 m1Store
  in  (TVI (m1Res - m2Res), m2Store)
interpMathOp (I intConst) store = (TVI intConst, store)
interpMathOp (IV intVar) store =
  case (lookup intVar store) of
    Nothing -> error ("variable: " ++ intVar ++ " not bound")
    Just (TVI i) -> (TVI i, store)
    Just t -> error ("in variable lookup found something but expected boolean")

intAsBool :: Int -> Bool
intAsBool i = if i == 0 then False else True

typeValEq :: TypeVal -> TypeVal -> Bool
typeValEq (TVI t1) (TVI t2) = t1 == t2
typeValEq (TVB t1) (TVB t2) = t1 == t2
typeValEq t1 t2 = error ("comparing uncomparable types")

