module IntermediateCode where

import qualified Parser as P
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

type Table = Map String String
type Temp = String
type Label = String
type Count = (Int, Int)

type CGState = (Table, Count)

-- Retrieves the current symbol table from the state
getTable :: State CGState Table
getTable = gets fst

-- Updates the symbol table in the state
putTable :: Table -> State CGState ()
putTable t = modify (\(_, c) -> (t, c))

-- Retrieves the current count (for temps and labels) from the state
getCount :: State CGState Count
getCount = gets snd

-- Updates the count in the state
putCount :: Count -> State CGState ()
putCount c = modify (\(t, _) -> (t, c))

-- Generates a new temporary variable
newTemp :: State CGState Temp
newTemp = do
  (table, (t, l)) <- get
  let temp = "$t" ++ show t
  put (table, (t + 1, l))
  return temp

-- Decrements the temp count by a given number
popTemp :: Int -> State CGState ()
popTemp k = do
  (table, (t, l)) <- get
  put (table, (t - k, l))

-- Generates a new label
newLabel :: State CGState Label
newLabel = do
  (table, (t, l)) <- get
  let lbl = "L" ++ show l
  put (table, (t, l + 1))
  return lbl

-- Intermediate representation of instructions
data Instr
  = MOVE String String
  | MOVEI String Int
  | MOVES Label String
  | OP BinOp String String String
  | LABEL Label
  | JUMP Label
  | COND String RelOp String Label Label
  | CALL String String [String]
  | RETURN String
  | FUN String [String] [Instr]
  | INC String               -- Increment operation
  | DEC String               -- Decrement operation
  deriving (Show, Eq, Read)

-- Binary operations
data BinOp = Add | Minus | Times | Div | Mod
  deriving (Eq, Show, Read)

-- Relational operations
data RelOp = Less | LessEq | Greater | GreaterEq | Equals | NotEquals
  deriving (Eq, Show, Read)

-- Generates intermediate code for a program
generateCode :: P.Program -> State CGState [Instr]
generateCode (P.Begin tops) = generateTopLevelExprs tops

-- Generates intermediate code for top-level expressions
generateTopLevelExprs :: [P.TopLevelExpr] -> State CGState [Instr]
generateTopLevelExprs [] = return []
generateTopLevelExprs (x:xs) = do
  c1 <- generateTopLevelExpr x
  c2 <- generateTopLevelExprs xs
  return (c1 ++ c2)

-- Generates intermediate code for a single top-level expression
generateTopLevelExpr :: P.TopLevelExpr -> State CGState [Instr]
generateTopLevelExpr (P.TopLevelFunDecl f) = generateFunDecl f
generateTopLevelExpr (P.Statement s) = do
  dest <- newTemp
  code <- generateStmt s dest ""
  popTemp 1
  return code
generateTopLevelExpr _ = do
  error "Cannot find type"

-- Generates intermediate code for a function declaration
generateFunDecl :: P.FunDecl -> State CGState [Instr]
generateFunDecl (P.FunDeclaration ident params stmts) = do
  dest <- newTemp
  _ <- generateParams params
  code <- generateStmts stmts dest ""
  popTemp (length params + 1)
  return [FUN ident params (code ++ [RETURN dest])]

-- Generates intermediate code for function parameters
generateParams :: [String] -> State CGState [String]
generateParams [] = return []
generateParams (p:ps) = do
  table <- getTable
  putTable (Map.insert p p table)
  rest <- generateParams ps
  return (p:rest)

-- Generates intermediate code for a list of statements
generateStmts :: [P.Stmt] -> String -> Label -> State CGState [Instr]
generateStmts [] _ _ = return []
generateStmts (s:ss) d br = do
  c1 <- generateStmt s d br
  c2 <- generateStmts ss d br
  return (c1 ++ c2)

-- Generates intermediate code for a single statement
generateStmt :: P.Stmt -> String -> Label -> State CGState [Instr]
generateStmt (P.If cond stmt) dest br = do
  lt <- newLabel
  lf <- newLabel
  cc <- generateCond cond lt lf br
  ct <- generateStmts stmt dest br
  return (cc ++ [LABEL lt] ++ ct ++ [LABEL lf])

generateStmt (P.IfElse cond st f) dest br = do
  lt <- newLabel
  lf <- newLabel
  le <- newLabel
  cc <- generateCond cond lt lf br
  ct <- generateStmts st dest br
  cf <- generateStmts f dest br
  return (cc ++ [LABEL lt] ++ ct ++ [JUMP le, LABEL lf] ++ cf ++ [LABEL le])

generateStmt (P.While cond stmt) dest _ = do
  lc <- newLabel
  lb <- newLabel
  le <- newLabel
  cc <- generateCond cond lb le le
  cb <- generateStmts stmt dest le
  return ([LABEL lc] ++ cc ++ [LABEL lb] ++ cb ++ [JUMP lc, LABEL le])

generateStmt (P.Return expr) dest br = do
  code <- generateExpr expr dest br
  return (code ++ [RETURN dest])

generateStmt P.Break _ br = return [JUMP br]

generateStmt (P.ExprStmt e) dest br = generateExpr e dest br

generateStmt (P.TopLevelVarDecl b) dest br = generateVarDecl b dest br

generateStmt (P.TopLevelValDecl b) dest br = generateValDecl b dest br

-- Generates intermediate code for a variable declaration
generateVarDecl :: P.VarDecl -> String -> Label -> State CGState [Instr]
generateVarDecl (P.VarDeclaration ident _ expr) dest br = do
  temp <- newTemp
  codeExpr <- generateExpr expr temp ""
  table <- getTable
  putTable (Map.insert ident ident table)
  popTemp 1
  return (codeExpr ++ [MOVE ident temp])

-- Generates intermediate code for a value declaration
generateValDecl :: P.ValDecl -> String -> Label -> State CGState [Instr]
generateValDecl (P.ValDeclaration ident _ expr) dest br = do
  temp <- newTemp
  codeExpr <- generateExpr expr temp ""
  table <- getTable
  putTable (Map.insert ident ident table)
  popTemp 1
  return (codeExpr ++ [MOVE ident temp])

-- Generates intermediate code for an expression
generateExpr :: P.Expr -> String -> Label -> State CGState [Instr]
generateExpr (P.Var ident) dest _ = do
  table <- getTable
  case Map.lookup ident table of
    Just varName -> return [MOVE dest varName]
    Nothing -> error ("Variable " ++ ident ++ " not declared")

generateExpr (P.Assign ident expr) dest br = do
  code <- generateExpr expr dest br
  table <- getTable
  case Map.lookup ident table of
    Just _ -> return (code ++ [MOVE ident dest])
    Nothing -> error ("Variable " ++ ident ++ " not declared")

generateExpr (P.FuncCall ident args) dest br = do
  (cargs, temps) <- generateExprs args br
  popTemp (length temps)
  return (cargs ++ [CALL dest identwhil temps])

generateExpr (P.Op P.Add (P.Var ident) (P.IntVal 1)) dest br = do
  table <- getTable
  case Map.lookup ident table of
    Just varName -> return [INC varName]
    Nothing -> error ("Variable " ++ ident ++ " not declared")

generateExpr (P.Op P.Sub (P.Var ident) (P.IntVal 1)) dest br = do
  table <- getTable
  case Map.lookup ident table of
    Just varName -> return [DEC varName]
    Nothing -> error ("Variable " ++ ident ++ " not declared")

generateExpr (P.Parenthesis e) dest br = generateExpr e dest br

generateExpr (P.IntVal n) dest _ = return [MOVEI dest n]

generateExpr (P.DoubleVal n) dest _ = return [MOVEI dest (round n)]

generateExpr (P.StringVal s) dest _ = do
  lbl <- newLabel
  return [MOVES lbl s, MOVE dest lbl]

generateExpr (P.BoolLit True) dest _ = return [MOVEI dest 1]

generateExpr (P.BoolLit False) dest _ = return [MOVEI dest 0]

generateExpr (P.Op P.And e1 e2) dest br = do
  -- Logical AND
  lf <- newLabel
  le <- newLabel
  c1 <- generateCond e1 le lf br -- Evaluate the first condition
  c2 <- generateCond e2 dest lf br -- Evaluate the second condition if the first is true
  return (c1 ++ [LABEL le] ++ c2 ++ [LABEL lf, MOVEI dest 0, LABEL le, MOVEI dest 1])

generateExpr (P.Op P.Or e1 e2) dest br = do
  -- Logical OR
  lt <- newLabel
  le <- newLabel
  c1 <- generateCond e1 lt le br -- Evaluate the first condition
  c2 <- generateCond e2 dest le br -- Evaluate the second condition if the first is false
  return (c1 ++ [LABEL lt, MOVEI dest 1, JUMP le, LABEL le, MOVEI dest 0])

generateExpr (P.Not e) dest br = do
  t1 <- newTemp
  condCode <- generateCond e dest t1 br
  popTemp 1
  return (condCode ++ [LABEL t1, MOVEI dest 0, JUMP br, LABEL dest, MOVEI dest 1])

generateExpr (P.Negative e) dest br = do
  t1 <- newTemp
  c <- generateExpr e t1 br
  popTemp 1
  return (c ++ [OP Minus dest "0" t1])

generateExpr (P.Op op e1 e2) dest br =
  case opToBinOp op of
    Just bin -> do
      t1 <- newTemp
      t2 <- newTemp
      c1 <- generateExpr e1 t1 br
      c2 <- generateExpr e2 t2 br
      popTemp 2
      return (c1 ++ c2 ++ [OP bin dest t1 t2])
    Nothing -> case opToRelOp op of
      Just rel -> do
        lt <- newLabel
        lf <- newLabel
        le <- newLabel
        cc <- generateCond (P.Op op e1 e2) lt lf br
        return (cc ++ [LABEL lt, MOVEI dest 1, JUMP le, LABEL lf, MOVEI dest 0, LABEL le])
      Nothing -> error ("Unsupported operation: " ++ show op)

generateExpr expr _ _ = error ("Expression not handled: " ++ show expr)

-- Generates intermediate code for a list of expressions
generateExprs :: [P.Expr] -> Label -> State CGState ([Instr],[String])
generateExprs [] _ = return ([], [])
generateExprs (e:es) br = do
  temp <- newTemp
  c1 <- generateExpr e temp br
  (cr, ts) <- generateExprs es br
  return (c1 ++ cr, temp:ts)

-- Generates intermediate code for a conditional expression
generateCond :: P.Expr -> Label -> Label -> Label -> State CGState [Instr]
generateCond (P.Parenthesis e) lt lf br = generateCond e lt lf br
generateCond (P.Op op e1 e2) lt lf br =
  case opToRelOp op of
    Just r -> do
      t1 <- newTemp
      t2 <- newTemp
      c1 <- generateExpr e1 t1 br
      c2 <- generateExpr e2 t2 br
      popTemp 2
      return (c1 ++ c2 ++ [COND t1 r t2 lt lf])
    Nothing -> case op of
      P.And -> do
        la <- newLabel
        c1 <- generateCond e1 la lf br
        c2 <- generateCond e2 lt lf br
        return (c1 ++ [LABEL la] ++ c2)
      P.Or -> do
        lo <- newLabel
        c1 <- generateCond e1 lt lo br
        c2 <- generateCond e2 lt lf br
        return (c1 ++ [LABEL lo] ++ c2)
      _ -> error ("Unsupported conditional operation: " ++ show op)

generateCond (P.Not e) lt lf br = generateCond e lf lt br

generateCond (P.BoolLit True) lt _ _ = return [JUMP lt]

generateCond (P.BoolLit False) _ lf _ = return [JUMP lf]

generateCond (P.Var ident) lt lf _ = do
  table <- getTable
  case Map.lookup ident table of
    Just varName -> return [COND varName NotEquals "0" lt lf]
    Nothing -> error ("Variable " ++ ident ++ " not declared")

generateCond expr _ _ _ = error ("Conditional expression not handled: " ++ show expr)

-- Converts a parser operation to a binary operation
opToBinOp :: P.Op -> Maybe BinOp
opToBinOp P.Add = Just Add
opToBinOp P.Sub = Just Minus
opToBinOp P.Mul = Just Times
opToBinOp P.Div = Just Div
opToBinOp P.Mod = Just Mod
opToBinOp _ = Nothing

-- Converts a parser operation to a relational operation
opToRelOp :: P.Op -> Maybe RelOp
opToRelOp P.Eq = Just Equals
opToRelOp P.Neq = Just NotEquals
opToRelOp P.Lt = Just Less
opToRelOp P.Leq = Just LessEq
opToRelOp P.Gt = Just Greater
opToRelOp P.Geq = Just GreaterEq
opToRelOp _ = Nothing