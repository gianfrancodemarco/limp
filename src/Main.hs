module Main where
import Interpreter
import Grammar
import Parser
import Control.Exception

--  author: Gianfranco Demarco
--  This is the main module of the program. It is responsible of running the top-level layer of the interpreter, which show the menu and then executes the parsed code.
--  The execution modes are:
--    1) Run a program from the file; if the program is correctly executed, the state is retained and the interactive shell is launched
--    2) Run the interactive shell; the program can be written and executed line by line


-- main :: IO ()
main = do

      putStrLn formattedChoices
      tryChoice <- try (readLn :: IO Int) :: IO (Either SomeException Int)

      case tryChoice of
        Left ex -> tryAgain
        Right choice ->
          do
            case validate choice of
               -- when the choice is valid it is evaluated
               Just n  -> execute choice
               Nothing -> tryAgain

      where tryAgain = do
                         putStrLn "Please try again"
                         main

            format (i, (text, _)) = show i ++ ") " ++ text   -- displaying the choices in a certain string format
            formattedChoices = unlines (map format choices)


 -- Checks if the user has made a legal choice
validate :: Int -> Maybe Int
validate n | outOfBounds n = Nothing
           | otherwise     = Just n
           where
             outOfBounds n = (n < 1) || (n > length choices)


-- Returns the choices for the menu
-- choices :: [(Int, (String, IO ()))]
choices = zip [1, 2, 3] [
   ("Run a program from a file", readFromFile),
   ("Run the interactive shell", readFromCLI),
   ("Exit", esc )
 ]


-- executes the main
execute :: Int -> IO Env
execute n = head [f | (index, (text, f)) <- choices, index == n]


-- Section: choices

-- First option
readFromFile =
  do
  putStrLn "Enter the name of the file you want to run:"
  filename <- getLine;
  fileContent <- readFile filename
  newState <- parseAndExecute fileContent []
  _readFromCLI newState

-- Second option
-- When the interactive choice is chosen
readFromCLI =
  do
  putStrLn "Interactive shell"
  _readFromCLI []

-- Recursively called both from the shell mode and at the end of the execution of a program
_readFromCLI currentState =
  do
  putStr "limp> "
  do
    inputProgram <- getLine;
    if inputProgram /= "quit"
      then do
        newState <- parseAndExecute inputProgram currentState
        _readFromCLI newState
      else
        return currentState


-- Third option
esc =
  do
    print "Quitting the interpreter. Goodbye!"
    return []

-- End section choice


parseAndExecute :: String -> Env -> IO Env
parseAndExecute inputProgram currentState =
  do
    let parsed = parse inputProgram
    if parseFailed parsed
       then do
        putStr "\nParsing failed\n"
        putStr "\nRemaining input:"
        print (getRemainingInput parsed)
        return []
    else do
        putStrLn "\nParsing success!\n"
        let state = executeProgram currentState (getParsedCommands parsed)
        putStr "\nInput Program\n"
        putStr inputProgram
        putStr "\nRepresentation of the program:\n"
        print (getParsedCommands parsed)
        putStr "\nState of the memory:\n"
        print state
        return state
