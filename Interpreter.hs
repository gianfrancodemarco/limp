module Interpreter where
import Grammar

-- Alessia Laforgia, mat.742292
-- The Intepreter is the part of our program that takes in input the intermediate representation tree and gives it a meaning, that is exactly the semantic.

-- These are the components of the Environment

data Variable = Variable {name  :: String,
                          value :: Type } deriving Show

-- Array of tuples of type Variable

type Env = [Variable]

-- I define the operations to exploit the Env

-- This modifies the Environment after some modification to variables.

modifyEnv :: Env -> Variable -> Env 
modifyEnv [] var = [var]
modifyEnv (x:xs) newVar = if (name x) == (name newVar) then [
    newVar] ++ xs 
                        else[x] ++ modifyEnv xs newVar


-- This returns the value of the variable 

searchVariable:: Env -> String-> Maybe Type
searchVariable [] varname = Nothing
searchVariable (x:xs) varname = if (name x) == varname
        then Just (value x)
                                else searchVariable xs
                                varname


--ARITHMETIC EXPRESSION EVALUATION--

arithExprEval:: Env -> ArithExpr -> Maybe Int

arithExprEval env (Constant i) = Just i

arithExprEval env (ArithVariable i) = 
        case searchVariable env i of
                Just (IntType v)-> Just v
                Just _ -> error "type mismatch"
                Nothing -> error "undeclared variable"


arithExprEval env (Add a b) =  pure (+) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Sub a b) = pure (-) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Mul a b) = pure (*) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Div a b) = pure (div) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Power a b) = pure (^) <*> (arithExprEval env a) <*> (arithExprEval env b)



-- BOOLEAN EXPRESSION EVALUATION

boolExprEval :: Env -> BoolExpr -> Maybe Bool

boolExprEval env (Boolean b) = Just b

boolExprEval env (BoolVariable s)=
        case searchVariable env s of 
                Just (BoolType v) -> Just v
                Just _ -> error "type mismatch"
                Nothing -> error "undeclared variable"

boolExprEval env (Lt a b) = pure (<) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (Gt a b) = pure (>) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (Eq a b) = pure (==) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (Neq a b) = pure (/=) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (Lte a b) = pure (<=) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (Gte a b) = pure (>=) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (And a b) = pure (&&) <*> (boolExprEval env a) <*> (boolExprEval env b)

boolExprEval env (Or a b) = pure (||) <*> (boolExprEval env a) <*> (boolExprEval env b)

boolExprEval env (Not a) = not <$> boolExprEval env a

-- EXECUTION OF THE PROGRAM--

execProgr :: Env -> [Command] -> Env

execProgr e [] = e 

execProgr e  (Skip : cs) = execProgr e cs

execProgr e ((IfElse b nc nc') : cs) =
        case boolExprEval e b of
                Just True -> execProgr e (nc ++ cs)
                Just False-> execProgr e (nc'++ cs)
                Nothing -> error "Error if"


execProgr e ((Whiledo b nc) : cs) =
        case boolExprEval e b of
                Just True -> execProgr e (nc ++ [(Whiledo b nc)] ++ cs)
                Just False -> execProgr e cs
                Nothing -> error "Error while"

execProgr e ((ArithAssign s a) : cs ) =
        case searchVariable e s of
                Just (IntType _ ) -> execProgr (modifyEnv e var) cs
                               where var = Variable s (IntType z)
                                        where Just z = arithExprEval e a
                Just _ -> error "Type mismatch"
                Nothing -> error "Error assign" 


execProgr e ((BoolAssign s b) : cs ) =
        case searchVariable e s of
                Just (BoolType _ ) -> execProgr (modifyEnv e var) cs
                               where var = Variable s (BoolType z)
                                        where Just z = boolExprEval e b
                Just _ -> error "Type mismatch"
                Nothing -> error "Error assign" 

execProgr e (( ArithDeclare s a ) : cs ) =
        case arithExprEval e a of
                Just exp -> case searchVariable e s of
                        Just _ -> error "double declaration"
                        Nothing -> execProgr (modifyEnv e var) cs
                               where var = Variable s (IntType z)
                                        where Just z = arithExprEval e a
                Nothing -> error "Error declare"

execProgr e (( BoolDeclare s a ) : cs ) =
        case boolExprEval e a of
                Just exp -> case searchVariable e s of
                        Just _ -> error "double declaration"
                        Nothing -> execProgr (modifyEnv e var) cs
                               where var = Variable s (BoolType z)
                                        where Just z = boolExprEval e a
                Nothing -> error "Error declare"
