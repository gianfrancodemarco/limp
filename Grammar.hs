module Grammar where 

-- author: Gianfranco Demarco


data Type = 
          IntType Int      -- all the types that i can manage in my Interpreter
        | BoolType Bool
        | ArrayType [Int]
        deriving Show


data ArithExpr =              -- all expression that give Integer as result
          Constant Int
        | ArithVariable String
        | ArrVariable String ArithExpr
        | Add ArithExpr ArithExpr
        | Sub ArithExpr ArithExpr
        | Mul ArithExpr ArithExpr
        | Div ArithExpr ArithExpr
        | Power ArithExpr ArithExpr
        deriving Show

data ArrayExpr =                -- all expression that give Array as result
        Array [ArithExpr]
       | ArrayVariable String
       deriving Show


data BoolExpr =                -- all expression that give Bool as result
          Boolean Bool
        | BoolVariable String
        | Lt ArithExpr ArithExpr
        | Gt ArithExpr ArithExpr
        | Eq ArithExpr ArithExpr
        | Neq ArithExpr ArithExpr
        | Lte ArithExpr ArithExpr
        | Gte ArithExpr ArithExpr
        | And BoolExpr BoolExpr
        | Or BoolExpr BoolExpr
        | Not BoolExpr
        deriving Show

-- This defines the commands i'm going to use in my language
data Command =
          Skip
        | IfElse BoolExpr [Command] [Command] -- probabile lista di comandi
        | While BoolExpr [Command]
        | ArithDeclare String ArithExpr
        | BoolDeclare String BoolExpr
        | ArrayDeclare String ArithExpr
        | ArithAssign String ArithExpr
        | BoolAssign String BoolExpr  
        | ArrayAssign String ArithExpr ArithExpr --nome array + indice + numero da mettere all'interno
  deriving Show

type Program = [Command] 