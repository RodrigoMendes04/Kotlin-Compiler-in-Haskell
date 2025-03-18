```haskell
module MachineCode where

import IntermediateCode
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import qualified Data.Text as Text

type Code = String
type NextReg = Int
type NextConst = Int
type GVar = Int
type Const = String

data RegVal = REG String
            | CONST Const
            deriving (Eq, Show)

type Mappings = Map String RegVal
type MappingsConst = Map String RegVal

-- Translates a list of intermediate instructions to machine code
transProgToCode :: [Instr] -> ([Code],[(String,RegVal)])
transProgToCode xs =
  let (i, c, m, cm, ci, g) = transToCode xs [".text"] 0 0 0 Map.empty Map.empty
      consts = Map.toList cm
      dotdata = dotData consts [".data"]
  in (dotdata ++ c, Map.toList m)

-- Translates intermediate instructions to machine code, maintaining state
transToCode :: [Instr] -> [Code] -> NextReg -> NextConst -> GVar -> Mappings -> MappingsConst
            -> (NextReg,[Code],Mappings,MappingsConst,NextConst,GVar)
transToCode [] c i ci g m cm = (i,c,m,cm,ci,g)

-- Translates MOVE instruction
transToCode ((MOVE d s):xs) c i ci g tabl ctabl =
  let (i1, c1, tabl1, ctabl1, ci1, g1, code) = moveInstr d s c i ci g tabl ctabl
  in transToCode xs code i1 ci1 g1 tabl1 ctabl1

-- Translates MOVEI instruction
transToCode ((MOVEI d n):xs) c i ci g tabl ctabl =
  let (i1, c1, tabl1, ctabl1, ci1, g1, code) = moveIInstr d n c i ci g tabl ctabl
  in transToCode xs code i1 ci1 g1 tabl1 ctabl1

-- Translates MOVES instruction
transToCode ((MOVES temp str):xs) c i ci g tabl ctabl =
  -- Store the string in ctabl
  let ctabl1 = Map.insert temp (CONST str) ctabl
  in transToCode xs c i (ci+1) g tabl ctabl1

-- Translates OP instruction
transToCode ((OP op d s1 s2):xs) c i ci g tabl ctabl =
  let (i1, c1, tabl1, ctabl1, ci1, g1, code) = opInstr op d s1 s2 c i ci g tabl ctabl
  in transToCode xs code i1 ci1 g1 tabl1 ctabl1

-- Translates LABEL instruction
transToCode ((LABEL l):xs) c i ci g tabl ctabl =
  transToCode xs (consecutiveCodeInserts c [l ++ ":"]) i ci g tabl ctabl

-- Translates JUMP instruction
transToCode ((JUMP l):xs) c i ci g tabl ctabl =
  transToCode xs (consecutiveCodeInserts c ["j " ++ l]) i ci g tabl ctabl

-- Translates COND instruction
transToCode ((COND t1 rel t2 lt lf):xs) c i ci g tabl ctabl =
  let (i1,c1,tabl1,ctabl1,ci1,g1,code) = condInstr t1 rel t2 lt lf c i ci g tabl ctabl
  in transToCode xs code i1 ci1 g1 tabl1 ctabl1

-- Translates CALL instruction
transToCode ((CALL d ident args):xs) c i ci g tabl ctabl =
  let (i1,c1,tabl1,ctabl1,ci1,g1,code) = callInstr d ident args c i ci g tabl ctabl
  in transToCode xs code i1 ci1 g1 tabl1 ctabl1

-- Translates RETURN instruction
transToCode ((RETURN d):xs) c i ci g tabl ctabl =
  let reg_d = giveMeRegister d tabl i
      (i1, preC, reg_d') = translateMapping d (fst reg_d) (Map.insert d (snd reg_d) tabl)
      code = consecutiveCodeInserts c [preC, "move $v0, " ++ reg_d', "jr $ra"]
  in transToCode xs code i1 ci g (Map.insert d (snd reg_d) tabl) ctabl

-- Translates FUN instruction
transToCode ((FUN l params ins):xs) c i ci g tabl ctabl =
  let pre = [l ++ ":", "sw $ra, 0($sp)", "sw $fp, -4($sp)", "move $fp, $sp", "addiu $sp, $sp, -32"]
      post = ["lw $ra, 0($fp)", "lw $fp, -4($fp)", "move $sp, $fp", "jr $ra"]
      (i1, c1, m1, cm1, ci1, g1) = transToCode ins (consecutiveCodeInserts c pre) i ci g tabl ctabl
      code = consecutiveCodeInserts c1 post
  in transToCode xs code i1 ci1 g1 m1 cm1

-- Translates INC instruction
transToCode ((INC d):xs) c i ci g tabl ctabl =
  let reg_d = giveMeRegister d tabl i
      (i1, preC, reg_d') = translateMapping d (fst reg_d) (Map.insert d (snd reg_d) tabl)
      code = consecutiveCodeInserts c [preC, "addi " ++ reg_d' ++ ", " ++ reg_d' ++ ", 1"]
  in transToCode xs code i1 ci g (Map.insert d (snd reg_d) tabl) ctabl

-- Translates DEC instruction
transToCode ((DEC d):xs) c i ci g tabl ctabl =
  let reg_d = giveMeRegister d tabl i
      (i1, preC, reg_d') = translateMapping d (fst reg_d) (Map.insert d (snd reg_d) tabl)
      code = consecutiveCodeInserts c [preC, "addi " ++ reg_d' ++ ", " ++ reg_d' ++ ", -1"]
  in transToCode xs code i1 ci g (Map.insert d (snd reg_d) tabl) ctabl

-- Helper function for MOVE instruction
moveInstr :: String -> String -> [Code] -> NextReg -> NextConst -> GVar -> Mappings -> MappingsConst
          -> (NextReg,[Code],Mappings,MappingsConst,NextConst,GVar,[Code])
moveInstr d s c i ci g tabl ctabl =
  let reg_d = giveMeRegister d tabl i
      reg_s = giveMeRegister s (Map.insert d (snd reg_d) tabl) (fst reg_d)
      tabl1 = consecutiveInserts [d,s] [reg_d,reg_s] tabl
      (_, preD, rd) = translateMapping d (fst reg_s) tabl1
      (_, preS, rs) = translateMapping s (fst3 (translateMapping d (fst reg_s) tabl1)) tabl1
      code = consecutiveCodeInserts c [preD, preS, "move " ++ rd ++ ", " ++ rs]
  in (fst3 (translateMapping s (fst3 (translateMapping d (fst reg_s) tabl1)) tabl1), code, tabl1, ctabl, ci, g, code)

-- Helper function for MOVEI instruction
moveIInstr :: String -> Int -> [Code] -> NextReg -> NextConst -> GVar -> Mappings -> MappingsConst
           -> (NextReg,[Code],Mappings,MappingsConst,NextConst,GVar,[Code])
moveIInstr d n c i ci g tabl ctabl =
  let reg_d = giveMeRegister d tabl i
      tabl1 = Map.insert d (snd reg_d) tabl
      (_, preD, rd) = translateMapping d (fst reg_d) tabl1
      code = consecutiveCodeInserts c [preD, "li " ++ rd ++ ", " ++ show n]
  in (fst3 (translateMapping d (fst reg_d) tabl1), code, tabl1, ctabl, ci, g, code)

-- Helper function for OP instruction
opInstr :: BinOp -> String -> String -> String -> [Code] -> NextReg -> NextConst -> GVar -> Mappings -> MappingsConst
        -> (NextReg,[Code],Mappings,MappingsConst,NextConst,GVar,[Code])
opInstr op d s1 s2 c i ci g tabl ctabl =
  let reg_d = giveMeRegister d tabl i
      reg_s1 = giveMeRegister s1 (Map.insert d (snd reg_d) tabl) (fst reg_d)
      reg_s2 = giveMeRegister s2 (consecutiveInserts [d,s1] [reg_d,reg_s1] tabl) (fst reg_s1)
      tabl1 = consecutiveInserts [d,s1,s2] [reg_d,reg_s1,reg_s2] tabl
      (_, preD, rd) = translateMapping d (fst reg_s2) tabl1
      (_, preS1, rs1) = translateMapping s1 (fst3 (translateMapping d (fst reg_s2) tabl1)) tabl1
      (_, preS2, rs2) = translateMapping s2 (fst3 (translateMapping s1 (fst3 (translateMapping d (fst reg_s2) tabl1)) tabl1)) tabl1
      mipsOp = case op of
                 Add -> "add"
                 Minus -> "sub"
                 Times -> "mul"
                 Div -> "div"
                 Mod -> "div"
      code = if op == Mod
             then consecutiveCodeInserts c [preD,preS1,preS2,mipsOp ++ " " ++ rs1 ++ ", " ++ rs2, "mfhi " ++ rd]
             else consecutiveCodeInserts c [preD,preS1,preS2,mipsOp ++ " " ++ rd ++ ", " ++ rs1 ++ ", " ++ rs2]
  in (fst3 (translateMapping s2 (fst3 (translateMapping s1 (fst3 (translateMapping d (fst reg_s2) tabl1)) tabl1)) tabl1), code, tabl1, ctabl, ci, g, code)

-- Helper function for COND instruction
condInstr :: String -> RelOp -> String -> Label -> Label -> [Code] -> NextReg -> NextConst -> GVar -> Mappings -> MappingsConst
          -> (NextReg,[Code],Mappings,MappingsConst,NextConst,GVar,[Code])
condInstr t1 rel t2 lt lf c i ci g tabl ctabl =
  let reg_t1 = giveMeRegister t1 tabl i
      reg_t2 = giveMeRegister t2 (Map.insert t1 (snd reg_t1) tabl) (fst reg_t1)
      tabl1 = consecutiveInserts [t1,t2] [reg_t1,reg_t2] tabl
      (_, preT1, rt1) = translateMapping t1 (fst reg_t2) tabl1
      (_, preT2, rt2) = translateMapping t2 (fst3 (translateMapping t1 (fst reg_t2) tabl1)) tabl1
      branch = case rel of
        Equals -> ["beq " ++ rt1 ++ ", " ++ rt2 ++ ", " ++ lt, "j " ++ lf]
        Less   -> ["blt " ++ rt1 ++ ", " ++ rt2 ++ ", " ++ lt, "j " ++ lf]
        LessEq -> ["ble " ++ rt1 ++ ", " ++ rt2 ++ ", " ++ lt, "j " ++ lf]
        Greater -> ["bgt " ++ rt1 ++ ", " ++ rt2 ++ ", " ++ lt, "j " ++ lf]
        GreaterEq -> ["bge " ++ rt1 ++ ", " ++ rt2 ++ ", " ++ lt, "j " ++ lf]
        NotEquals -> ["bne " ++ rt1 ++ ", " ++ rt2 ++ ", " ++ lt, "j " ++ lf]
      code = consecutiveCodeInserts c [preT1, preT2] ++ branch
  in (fst3 (translateMapping t2 (fst3 (translateMapping t1 (fst reg_t2) tabl1)) tabl1), code, tabl1, ctabl, ci, g, code)

-- Helper function for CALL instruction
callInstr :: String -> String -> [String] -> [Code] -> NextReg -> NextConst -> GVar -> Mappings -> MappingsConst
          -> (NextReg,[Code],Mappings,MappingsConst,NextConst,GVar,[Code])
callInstr d ident args c i ci g tabl ctabl =
  -- Simple calling convention: push args on stack, call jal, move ret to d
  let (argRegs, i2, tabl1) = loadArgs args tabl i
      reg_d = giveMeRegister d tabl1 i2
      tabl2 = Map.insert d (snd reg_d) tabl1
      (_, preD, rd) = translateMapping d (fst reg_d) tabl2
      code = consecutiveCodeInserts c (argRegs ++ ["jal " ++ ident, "move " ++ rd ++ ", $v0"])
  in (fst3 (translateMapping d (fst reg_d) tabl2), code, tabl2, ctabl, ci, g, code)

-- Helper function to load arguments into registers
-- carrega uma lista de argumentos em registers
loadArgs :: [String] -> Mappings -> NextReg -> ([Code],NextReg,Mappings)
loadArgs [] tabl i = ([], i, tabl)
loadArgs (x:xs) tabl i =
  let reg_x = giveMeRegister x tabl i
      tabl1 = Map.insert x (snd reg_x) tabl
      (_, preX, rx) = translateMapping x (fst reg_x) tabl1
      -- push argument on stack:
      code = [preX, "addi $sp, $sp, -4", "sw " ++ rx ++ ", 0($sp)"]
      (rest,i2,tabl2) = loadArgs xs tabl1 (fst3 (translateMapping x (fst reg_x) tabl1))
  in (code ++ rest, i2, tabl2)

-- Generates .data section for constants
-- gera a .data para constantes
dotData :: [(String,RegVal)] -> [Code] -> [Code]
dotData [] code = code
dotData ((lbl, CONST str):xs) ys =
  dotData xs (ys ++ [lbl ++ ": .asciiz \"" ++ str ++ "\""])
dotData (_:xs) ys = dotData xs ys

-- Allocates a register for a variable
-- aloca um registrador para uma variável
giveMeRegister :: String -> Mappings -> NextReg -> (NextReg, RegVal)
giveMeRegister t tabl idx =
  case Map.lookup t tabl of
    Nothing -> (idx+1, REG (registerList !! idx))
    Just (REG r) -> (idx, REG r)
    Just (CONST _) -> (idx+1, REG (registerList !! idx)) -- load const if needed separately

-- Translates a variable to its register or constant mapping
-- traduz uma variável para seu mapeamento de registrador ou constante
translateMapping :: String -> NextReg -> Mappings -> (NextReg,String,String)
translateMapping t idx tabl =
  case Map.lookup t tabl of
    Just (REG tm) -> (idx, "", tm)
    Just (CONST _) -> (idx, "", t) -- We'll handle loading elsewhere if needed
    Nothing -> error ("translateMappings - no mapping for " ++ t)

-- Inserts multiple mappings consecutively
-- insere múltiplos mapeamentos consecutivamente
consecutiveInserts :: [String] -> [(NextReg,RegVal)] -> Mappings -> Mappings
consecutiveInserts [] [] tabl = tabl
consecutiveInserts (x:xs) (y:ys) tabl = consecutiveInserts xs ys (Map.insert x (snd y) tabl)
consecutiveInserts _ _ _ = error "Non matching mappings in consecutive inserts"

-- Inserts multiple code lines consecutively
-- insere múltiplas linhas de código consecutivamente
consecutiveCodeInserts :: [Code] -> [Code] -> [Code]
consecutiveCodeInserts xs ys = xs ++ filter (/= "") ys

-- List of available registers
registerList :: [String]
registerList = ["$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7", "$t8", "$t9",
                "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6"]

-- Helper function to get the first element of a tuple
fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x
```