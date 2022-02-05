module Grammar where 

-- author: Gianfranco Demarco


data Type = 
          IntType Int      -- all the types that i can manage in my Interpreter
        | BoolType Bool
        | ArrayType [Int]
        | StackType [Int]
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
        deriving Show

-- This defines the commands i'm going to use in my language
data Command =
          Skip
        | IfElse BoolExpr [Command] [Command] -- probabile lista di comandi
        | While BoolExpr [Command]
        | ArithDeclare String ArithExpr
        | BoolDeclare String BoolExpr
        | ArithAssign String ArithExpr
        | BoolAssign String BoolExpr
        | ArrayDeclare String ArithExpr                  -- array a[5];
        | ArrayDeclareFullAssign String [ArithExpr]      -- array a[5] = [1,2,3,4,5];
        | ArrayAssign String ArithExpr ArithExpr         -- array, index, value
        | ArrayFullAssign String [ArithExpr]
        | ArrayFromConcat String String String           -- destination = concat arr1 arr2; create an array from the concatanation of two other arrays
        | ArrayDotProduct String String String
        | ArrayScalarProduct String String ArithExpr
        | StackDeclare String
        | StackPush String ArithExpr
        | StackPop String

  deriving Show

type Program = [Command] 