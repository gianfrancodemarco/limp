module Main where

import System.IO

import Parser (program, parse, parseProgram)
import Types (Variable(..), Env)

--------------------------------------------------------------
--------------------------------------------------------------
-- EXECUTION OF THE PROGRAM
--------------------------------------------------------------
fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

exec :: [(Env, String, String)] -> ([Env],String)

-- case: empty list
exec [] = ([],"[ERROR] Invalid input!\n")

-- case: entire input string consumed
exec [(env, parsedString, "")] = ([newEnv],
    "Parsed code: \n\n " ++ parsedString ++ "\n\n" ++
    "Memory: \n\n" ++ (getMemory parsed))
    where
        parsed = (parse program env parsedString)
        newEnv = if (length parsed) > 0 then fst3 (parsed !! 0)
                 else env

-- case: input string not entirely consumed
exec [(env, parsedString, leftString)] = ([env],
    "Parsed code: \n\n" ++ parsedString ++ "\n\n" ++
    "Memory: \n\n" ++ (getMemory (parse program [] parsedString)) ++
    "Error: \n\n Unused input '" ++ leftString ++ "'\n")

getMemory :: [(Env, String, String)] -> String
getMemory [] = "Invalid input\n"

getMemory [(x:xs, parsedString, "")] =
    "Integer: " ++ (name x) ++ " = " ++ (show (value x)) ++ "\n" ++
    (getMemory    [(xs,parsedString,"")])

getMemory [(env, parsedString, leftString)] = case leftString of
    "" -> ""
    otherwise -> "Error (unused input '" ++ leftString ++ "')\n" ++ getMemory [(env,parsedString, "")]


-- Interpreter Interface
logo :: IO String
logo = do putStrLn ""
          putStrLn "   ██╗  ██╗███╗   ███╗██████╗ "
          putStrLn "   ██║  ██║████╗ ████║██╔══██╗"
          putStrLn "   ██║  ██║██╔████╔██║██████╔╝"
          putStrLn "   ██║  ██║██║╚██╔╝██║██╔═══╝ "
          putStrLn "   ████║██║██║ ╚═╝ ██║██║     "
          putStrLn "   ╚═══╝╚═╝╚═╝     ╚═╝╚═╝  "
          putStrLn "  Light IMPerative"
          putStrLn "   Language Interpreter"
          putStrLn " by Gianfranco Demarco\n"
          putStrLn "Enter the code to be evaluated,\nor type 'exit' to quit."
          menu [];

menu :: [Env] -> IO String
menu [] = menu [[]]
menu [env] = do {putStr "LIMP> ";
             hFlush stdout;
             input <- getLine;
 
             if (input == "clear") then menu [[]] else 
             if (input == "exit") then return "Bye!";
             else do let res = (exec (parse parseProgram env input))
                     putStrLn (snd res)
                     menu (fst res)
                
            }
  
main = logo;
