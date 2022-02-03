module Interpreter where
import Grammar
import Array

-- author: Gianfranco Demarco
-- The Interpreter is the module that takes in input the intermediate representation tree of the program and extracts the semantics


-- The Environment, representing the state of the program, is an array of Variables
-- A Variable is an object defined by a name::string and a value::Type

data Variable = Variable {name :: String, value :: Type } deriving Show
type Env = [Variable]



-- We need two operations for the Env:
-- read: returns the value of a variable in the store if present, else raise an error
-- write: writes or updates the value of a variable in the store

writeEnv :: Env -> Variable -> Env
writeEnv [] var = [var]
writeEnv (x:xs) var | (name x == name var) = [var] ++ xs              -- If we find the var, replace with the new one
                    | otherwise            = [x] ++ writeEnv xs var


-- This returns the value of the variable 

readEnv:: Env -> String-> Maybe Type
readEnv [] varName = Nothing
readEnv (x:xs) varName | name x == varName = Just (value x)
                       | otherwise         = readEnv xs varName


--ARITHMETIC EXPRESSION EVALUATION--

arithExprEval:: Env -> ArithExpr -> Maybe Int

arithExprEval env (Constant i) = Just i

arithExprEval env (ArithVariable i) = 
        case readEnv env i of
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
        case readEnv env s of
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



-- Program Flow --

executeProgram :: Env -> [Command] -> Env

-- Execute nothing, env unaltered
executeProgram env [] = env

-- Execute skip --> execute rest of commands
executeProgram env (Skip : restOfCommands) = executeProgram env restOfCommands


executeProgram env ((IfElse predicate ifBranch elseBranch) : restOfCommands) =
        case boolExprEval env predicate of
                Just True -> executeProgram env (ifBranch ++ restOfCommands)
                Just False-> executeProgram env (elseBranch ++ restOfCommands)
                Nothing -> error "Error on IfElse evaluation"


executeProgram env ((While predicate whileBody) : restOfCommands) =
        case boolExprEval env predicate of
                Just True -> executeProgram env (whileBody ++ [(While predicate whileBody)] ++ restOfCommands)
                Just False -> executeProgram env restOfCommands
                Nothing -> error "Error while"

executeProgram env ((ArithAssign identifier aExp) : restOfCommands) =
        case readEnv env identifier of
                Just (IntType _ ) -> executeProgram (writeEnv env var) restOfCommands
                               where var = Variable identifier (IntType evaluated)
                                        where Just evaluated = arithExprEval env aExp
                Just _ -> error "Type mismatch in ArithAssign"
                Nothing -> error "Error in ArithAssign"


executeProgram env ((BoolAssign identifier bExp) : restOfCommands) =
        case readEnv env identifier of
                Just (BoolType _ ) -> executeProgram (writeEnv env var) restOfCommands
                               where var = Variable identifier (BoolType evaluated)
                                        where Just evaluated = boolExprEval env bExp
                Just _ -> error "Type mismatch in BoolAssign"
                Nothing -> error "Error in BoolAssign"

executeProgram env (( ArithDeclare identifier aExp ) : restOfCommands ) =
        case arithExprEval env aExp of
                Just exp -> case readEnv env identifier of
                        Just _ -> error "double ArithDeclare"
                        Nothing -> executeProgram (writeEnv env var) restOfCommands
                               where var = Variable identifier (IntType evaluated)
                                        where Just evaluated = arithExprEval env aExp
                Nothing -> error "Error in ArithDeclare"

executeProgram env (( BoolDeclare identifier bExp ) : restOfCommands ) =
        case boolExprEval env bExp of
                Just exp -> case readEnv env identifier of
                        Just _ -> error "double BoolDeclare"
                        Nothing -> executeProgram (writeEnv env var) restOfCommands
                               where var = Variable identifier (BoolType evaluated)
                                        where Just evaluated = boolExprEval env bExp
                Nothing -> error "Error in BoolDeclare"


-- array a[5];
executeProgram env (( ArrayDeclare identifier lengthaExp ) : restOfCommands ) =
         case readEnv env identifier of
                Just _ -> error "double ArrayDeclare"
                Nothing -> executeProgram (writeEnv env var) restOfCommands
                        where var = Variable identifier (ArrayType (getFilledArray length))
                              where Just length = arithExprEval env lengthaExp


-- a[1] = 2;
executeProgram env (( ArrayAssign identifier indexaExp valueaExp ) : restOfCommands ) =
        case readEnv env identifier of
              Just (ArrayType array) -> executeProgram (writeEnv env var) restOfCommands
                  where var = Variable identifier (ArrayType (replaceElemAt array index value))
                              where
                                    Just index = arithExprEval env indexaExp
                                    Just value = arithExprEval env valueaExp
              Just _ -> error "Type mismatch in ArrayAssign"
              Nothing -> error "Trying to assign to an array that has not been declared"

-- a = [1,2,3,4];
executeProgram env (( ArrayFullAssign identifier valuesaExps ) : restOfCommands ) =
        case readEnv env identifier of
              Just (ArrayType array) -> executeProgram (writeEnv env var) restOfCommands
                  where var = Variable identifier (ArrayType values)
                              where values = map fromJust (map (arithExprEval env) valuesaExps)
                                             where fromJust (Just x) = x
              Just _ -> error "Type mismatch in ArrayAssign"
              Nothing -> error "Trying to assign to an array that has not been declared"


-- array a[4] = [1,2,3,4];
executeProgram env (( ArrayDeclareFullAssign identifier valuesaExps ) : restOfCommands ) =
        case readEnv env identifier of
              Just _ -> error "double ArrayDeclareFullAssign"
              Nothing -> executeProgram (writeEnv env var) restOfCommands
                  where var = Variable identifier (ArrayType values)
                              where values = map fromJust (map (arithExprEval env) valuesaExps)
                                             where fromJust (Just x) = x


-- destination = concat arr1 arr2;
executeProgram env (( ArrayFromConcat destination headArray tailArray ) : restOfCommands ) =
        case readEnv env destination of
              Just _ -> error "ArrayFromConcat destination already in env"
              Nothing -> case readEnv env headArray of
                  Nothing -> error "headArray not in env"
                  Just (ArrayType headArrayValues) ->
                      case readEnv env tailArray of
                          Nothing -> error "tailArray not in env"
                          Just (ArrayType tailArrayValues) -> executeProgram (writeEnv env var) restOfCommands
                                  where var = Variable destination (ArrayType values)
                                              where values = headArrayValues ++ tailArrayValues
                          Just _ -> error "type mismatch for tailArray"
                  Just _ -> error "type mismatch for headArray"

-- destination = dot arr1 arr2;
executeProgram env (( ArrayFromDotProduct destination headArray tailArray ) : restOfCommands ) =
        case readEnv env destination of
              Just _ -> error "ArrayFromDotProduct destination already in env"
              Nothing -> case readEnv env headArray of
                  Nothing -> error "headArray not in env"
                  Just (ArrayType headArrayValues) ->
                      case readEnv env tailArray of
                          Nothing -> error "tailArray not in env"
                          Just (ArrayType tailArrayValues) ->
                                  do
                                    if length headArrayValues == length tailArrayValues
                                      then do
                                          let values = [x * y | (x,y) <- (zip headArrayValues tailArrayValues)]
                                          let var = Variable destination (ArrayType values)
                                          executeProgram (writeEnv env var) restOfCommands
                                      else error "length mismatch between headArray and tailArray"
                          Just _ -> error "type mismatch for tailArray"
                  Just _ -> error "type mismatch for headArray"


-- destination = dot arr1 3;
executeProgram env (( ArrayFromScalarProduct destination source scalaraExp ) : restOfCommands ) =
        case readEnv env destination of
              Just _ -> error "ArrayFromScalarProduct destination already in env"
              Nothing -> case readEnv env source of
                  Nothing -> error "source not in env"
                  Just (ArrayType sourceArrayValues) -> executeProgram (writeEnv env var) restOfCommands
                      where var = Variable destination (ArrayType values)
                                  where values = [x * scalar | x <- sourceArrayValues]
                                                 where Just scalar = arithExprEval env scalaraExp
                  Just _ -> error "type mismatch for headArray"
