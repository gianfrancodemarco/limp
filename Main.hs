module Main where
import Interpreter
import Grammar
import Parser
import Control.Exception

--  author: Gianfranco Demarco
--  This is the Mani module of the program. It is responsible of running the top-level layer of the interpreter, which show the menu and then executes the parsed code.
--  The execution modes are:
--    1) Read the program from a file;
--    2) Read the program from cli as a one liner.


-- Returns the choices for the menu
choices :: [(Int, (String, IO ()))]
choices = zip [1, 2, 3] [
   ("Run a program from a file", readFromFile),
   ("Write your program in a line", readFromCLI),
   ("Exit", esc )
 ]

 -- Checks if the user has made a legal choice
validate :: Int -> Maybe Int
validate n | outOfBounds n = Nothing
           | otherwise     = Just n
           where
             outOfBounds n = (n < 1) || (n > length choices)


-- executes the main
execute :: Int -> IO ()
execute n = doExec $ filter (\(i, _) -> i == n) choices
   where doExec ((_, (_,f)):_) = f




-- Section: choice


-- First option

readFromFile =
  do     
  putStrLn "Enter the name of the file you want to run:"
  filename <- getLine;
  fileContent <- readFile filename
  let parsed = parse fileContent
  if parseFailed parsed
    then do
      putStrLn "\nParsing failed\n"
      putStrLn "\nRemaining input:\n"
      print (getRemainingInput parsed)
  else do
      putStrLn "\nParsing success!\n"
      let state = executeProgram [] (getParsedCommands parsed)
      putStrLn "\nInput Program\n"
      putStrLn fileContent
      putStrLn "\nRepresentation of the program:\n"
      print (getParsedCommands parsed)
      putStrLn "\nState of the memory:\n"
      print state


-- Second option
readFromCLI =
  do
  putStrLn "Write a program all in one line:"
  inputProgram <- getLine;
  let parsed = parse inputProgram
  if parseFailed parsed
     then do
      putStr "\nParsing failed\n"
      putStr "\nRemaining input:"
      print (getRemainingInput parsed)
  else do
      putStrLn "\nParsing success!\n"
      let state = executeProgram [] (getParsedCommands parsed)
      putStr "\nInput Program\n"
      putStr inputProgram
      putStr "\nRepresentation of the program:\n"
      print (getParsedCommands parsed)
      putStr "\nState of the memory:\n"
      print state

-- Third option
esc = do error "Exit from the program! Goodbye!"

-- End section choice

main :: IO ()
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