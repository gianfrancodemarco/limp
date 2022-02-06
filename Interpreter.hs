module Interpreter where
import Grammar
import Array

-- author: Gianfranco Demarco
-- The Interpreter is the module that takes in input the intermediate representation tree of the program and extracts the semantics


-- The Environment, representing the state of the program, is an array of Variables
-- A Variable is an object defined by a name::string and a value::Type

data Variable = Variable {name :: String, value :: Type }
instance Show Variable where
  show x = "\n" ++ (name x) ++ " = " ++ (show (value x))

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

arithExprEval env (StackTop i) =
        case readEnv env i of
                Just (StackType (v:vs)) -> Just v
                Just _ -> error "type mismatch in StackTop"
                Nothing -> error "undeclared variable in StackTop"

arithExprEval env (QueueFirst i) =
        case readEnv env i of
                Just (QueueType (v:vs)) -> Just v
                Just _ -> error "type mismatch in QueueFirst"
                Nothing -> error "undeclared variable in QueueFirst"


arithExprEval env (Add a b) =  pure (+) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Sub a b) = pure (-) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Mul a b) = pure (*) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Div a b) = pure (div) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Power a b) = pure (^) <*> (arithExprEval env a) <*> (arithExprEval env b)



-- BOOLEAN EXPRESSION EVALUATION

boolExprEval :: Env -> BoolExpr -> Maybe Bool

boolExprEval env (Boolean b) = Just b

boolExprEval env (BoolVariable s) =
        case readEnv env s of
                Just (BoolType v) -> Just v
                Just _ -> error "type mismatch"
                Nothing -> error "undeclared variable"

boolExprEval env (StackEmpty s) =
        case readEnv env s of
                Just (StackType []) -> (Just True)
                Just (StackType _) -> (Just False)
                Just _ -> error "type mismatch"
                Nothing -> error "undeclared variable"

boolExprEval env (QueueEmpty s) =
        case readEnv env s of
                Just (QueueType []) -> (Just True)
                Just (QueueType _) -> (Just False)
                Just _ -> error "type mismatch in QueueEmpty"
                Nothing -> error "undeclared variable in QueueEmpty"


boolExprEval env (Lt a b) = pure (<) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (Gt a b) = pure (>) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (Eq a b) = pure (==) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (Neq a b) = pure (/=) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (Lte a b) = pure (<=) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (Gte a b) = pure (>=) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (And a b) = pure (&&) <*> (boolExprEval env a) <*> (boolExprEval env b)

boolExprEval env (Or a b) = pure (||) <*> (boolExprEval env a) <*> (boolExprEval env b)

boolExprEval env (Not a) = not <$> boolExprEval env a



-- ARRAY EXPRESSION EVALUATION

arrayExprEval :: Env -> ArrayExpr -> Maybe [Int]
arrayExprEval env (ArrayScalarProduct source scalaraExp) =
  case readEnv env source of
        Just (ArrayType sourceArrayValues) -> Just values
              where values = [x * scalar | x <- sourceArrayValues]
                     where Just scalar = arithExprEval env scalaraExp
        Just _ -> error "Type mismatch in arrayExprEval ArrayScalarProduct"
        Nothing -> error "Undeclared source in arrayExprEval ArrayScalarProduct"

arrayExprEval env (ArrayInit lengthaExp) = Just values
  where values = getFilledArray length
            where Just length = arithExprEval env lengthaExp

-- array a = [1,2,3,4];
arrayExprEval env (ArrayFull valuesaExps) = Just values
  where values = map fromJust (map (arithExprEval env) valuesaExps)
                 where fromJust (Just x) = x


-- concat a b OR a ++ b
arrayExprEval env (ArrayConcat headArray tailArray) =
  case readEnv env headArray of
                    Nothing -> error "headArray not in env in ArrayConcat"
                    Just (ArrayType headArrayValues) ->
                        case readEnv env tailArray of
                            Nothing -> error "tailArray not in env in ArrayConcat"
                            Just (ArrayType tailArrayValues) -> Just (headArrayValues ++ tailArrayValues)
                            Just _ -> error "type mismatch for tailArray in ArrayConcat"
                    Just _ -> error "type mismatch for headArray in ArrayConcat"


-- destination = dot arr1 arr2;
arrayExprEval env (ArrayDotProduct headArray tailArray) =
  case readEnv env headArray of
      Nothing -> error "headArray not in env in ArrayDotProduct"
      Just (ArrayType headArrayValues) ->
          case readEnv env tailArray of
              Nothing -> error "tailArray not in env in ArrayDotProduct"
              Just (ArrayType tailArrayValues) ->
                      do
                        if length headArrayValues == length tailArrayValues
                          then do
                              let values = [x * y | (x,y) <- (zip headArrayValues tailArrayValues)]
                              return values
                          else error "length mismatch between headArray and tailArray"
              Just _ -> error "type mismatch for tailArray"
      Just _ -> error "type mismatch for headArray"


-- Program Flow --

executeProgram :: Env -> [Command] -> Env

-- Execute nothing, env unaltered
executeProgram env [] = env

-- Execute skip --> execute rest of commands
executeProgram env (Skip: restOfCommands) = executeProgram env restOfCommands


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
                Nothing -> error "Identifier not in env for ArithAssign"


executeProgram env ((BoolAssign identifier bExp) : restOfCommands) =
        case readEnv env identifier of
                Just (BoolType _ ) -> executeProgram (writeEnv env var) restOfCommands
                               where var = Variable identifier (BoolType evaluated)
                                        where Just evaluated = boolExprEval env bExp
                Just _ -> error "Type mismatch in BoolAssign"
                Nothing -> error "Identifier not in env for BoolAssign"

executeProgram env (( ArithDeclare identifier aExp ) : restOfCommands ) =
        case arithExprEval env aExp of
                Just exp -> case readEnv env identifier of
                        Just (IntType _) -> error "Double ArithDeclare"
                        Just _ -> error "Type mismatch in ArithDeclare"
                        Nothing -> executeProgram (writeEnv env var) restOfCommands
                               where var = Variable identifier (IntType evaluated)
                                        where Just evaluated = arithExprEval env aExp
                Nothing -> error "Error in ArithDeclare"

executeProgram env (( BoolDeclare identifier bExp ) : restOfCommands ) =
        case boolExprEval env bExp of
                Just exp -> case readEnv env identifier of
                        Just (BoolType _) -> error "Double BoolDeclare"
                        Just _ -> error "Type mismatch in BoolDeclare"
                        Nothing -> executeProgram (writeEnv env var) restOfCommands
                               where var = Variable identifier (BoolType evaluated)
                                        where Just evaluated = boolExprEval env bExp
                Nothing -> error "Error in BoolDeclare"


-- array a[5];
executeProgram env (( ArrayDeclare identifier arrayExp ) : restOfCommands ) =
         case readEnv env identifier of
                Just (ArrayType array) -> error "double ArrayDeclare"
                Just _ -> error "Type mismatch for ArrayDeclare"
                Nothing -> executeProgram (writeEnv env var) restOfCommands
                        where var = Variable identifier (ArrayType values)
                              where Just values = arrayExprEval env arrayExp


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


executeProgram env (( ArrayFullAssign identifier arrayExp) : restOfCommands ) =
        case readEnv env identifier of
              Just (ArrayType array) -> executeProgram (writeEnv env var) restOfCommands
                        where var = Variable identifier (ArrayType values)
                              where Just values = arrayExprEval env arrayExp


-- stack myStack;
executeProgram env (( StackDeclare stackName ) : restOfCommands ) =
              case readEnv env stackName of
                  Just _ -> error "double StackDeclare"
                  Nothing -> executeProgram (writeEnv env var) restOfCommands
                      where var = Variable stackName (StackType [])


-- push myStack 3;
executeProgram env (( StackPush stackName valueaExp ) : restOfCommands ) =
              case readEnv env stackName of
                  Nothing -> error "stackName not in env"
                  Just (StackType stackValues) -> executeProgram (writeEnv env var) restOfCommands
                      where var = Variable stackName (StackType (value : stackValues))
                                  where Just value = arithExprEval env valueaExp
                  Just _ -> error "type mismatch for stackName in StackPush"


-- pop myStack;
executeProgram env (( StackPop stackName ) : restOfCommands ) =
              case readEnv env stackName of
                  Nothing -> error "stackName not in env"
                  Just (StackType (v:vs)) -> executeProgram (writeEnv env var) restOfCommands
                      where var = Variable stackName (StackType vs)
                  Just (StackType []) -> error "trying to pop from an empty stack"
                  Just _ -> error "type mismatch for stackName in StackPop"


-- queue myQueue;
executeProgram env (( QueueDeclare queueName ) : restOfCommands ) =
              case readEnv env queueName of
                  Just _ -> error "double QueueDeclare"
                  Nothing -> executeProgram (writeEnv env var) restOfCommands
                      where var = Variable queueName (QueueType [])

-- enqueue myQueue 3;
executeProgram env (( QueueEnqueue queueName valueaExp ) : restOfCommands ) =
              case readEnv env queueName of
                  Nothing -> error "queueName not in env in QueueEnqueue"
                  Just (QueueType queueValues) -> executeProgram (writeEnv env var) restOfCommands
                      where var = Variable queueName (QueueType (queueValues ++ [value]))
                                  where Just value = arithExprEval env valueaExp
                  Just _ -> error "type mismatch for queueName in QueueEnqueue"

executeProgram env (( QueueDequeue queueName ) : restOfCommands ) =
              case readEnv env queueName of
                  Nothing -> error "queueName not in env"
                  Just (QueueType (v : vs)) -> executeProgram (writeEnv env var) restOfCommands
                      where var = Variable queueName (QueueType (tail (v : vs)))
                  Just (QueueType []) -> error "trying to dequeue from an empty queue"
                  Just _ -> error "Type mismatch for queueName in QueueDequeue"
