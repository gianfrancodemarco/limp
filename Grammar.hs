module Grammar where 

-- author: Gianfranco Demarco


data Type = 
          IntType Int      -- all the types that i can manage in my Interpreter
        | BoolType Bool
        | ArrayType [Int]
        | StackType [Int]
        | QueueType [Int]
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
        | StackTop String
        | QueueFirst String
        | ArrayLength String
        | ArrayPos String ArithExpr
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
        | StackEmpty String
        | QueueEmpty String
        deriving Show

data ArrayExpr =
          ArrayInit ArithExpr
        | ArrayFull [ArithExpr]
        | ArrayConcat String String           -- destination = concat arr1 arr2; create an array from the concatanation of two other arrays
        | ArrayDotProduct String String
        | ArrayScalarProduct String ArithExpr
        deriving Show

-- This defines the commands i'm going to use in my language
data Command =
          Skip
        | IfElse BoolExpr [Command] [Command] -- probabile lista di comandi
        | While BoolExpr [Command]
        | BoolDeclare String BoolExpr
        | ArithDeclare String ArithExpr
        | BoolAssign String BoolExpr
        | ArithAssign String ArithExpr
        | ArrayDeclare String ArrayExpr                  -- array a[5];
        | ArrayAssign String ArithExpr ArithExpr         -- array, index, value
        | ArrayFullAssign String ArrayExpr         -- array, index, value
        | StackDeclare String
        | StackPush String ArithExpr
        | StackPop String
        | QueueDeclare String
        | QueueEnqueue String ArithExpr
        | QueueDequeue String
        deriving Show

type Program = [Command] 