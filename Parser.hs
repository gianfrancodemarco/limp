module Parser where
import Grammar
import Interpreter

-- Alessia Laforgia, mat.742292
{- The main purpose of the parser is to build a tree with all the expressions to interpretate. 
Once the tree is constructed, each character, each symbol, each element is associated to a certain semantics thanks to the interpreter -}

newtype Parser a = P (String -> [(a,String)])  

{-Functor, Applicative and Monad are classes already implemented in Prelude for the simple types. We need to implement them for the custom type
Parser. -}

instance Functor Parser where 
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g (P p) = P (\input -> case p input of 
        [] -> []
        [(v, out)] -> [(g v, out)] 
        ) 



instance Applicative Parser where                     
    --pure :: a -> Parser a 
    pure v = P(\input -> [(v, input)])                

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    (P pg) <*> px = P(\input -> case pg input of  
                [] -> []                              
                [(g, out)] -> case fmap g px of        
                                (P p) -> p out        
                )                                     
  
instance Monad Parser where                         
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (P p) >>= f = P (\input -> case p input of 
                    [] -> []                    -- fail if the application fails
                    [(v, out)] -> case f v of   -- apply f to the result v to give another parser f v 
                                (P p) -> p out
                    )
                    
class Monad f => Alternative f where  
  empty :: f a
  (<|>) :: f a -> f a -> f a
  many :: f a -> f [a]
  many x = some x <|> pure []

  some :: f a -> f [a]
  some x = pure (:) <*> x <*> many x

  chain :: f a -> f (a -> a -> a) -> f a          -- chain operator
  chain p op = do a <- p; rest a
        where
            rest a = (do f <- op; b <- p; rest (f a b)) <|> return a


instance Alternative Parser where     
  empty = P (const [])

  (P p) <|> (P q) =
    P( \input -> case p input of
          [] -> q input
          [(v, out)] -> [(v, out)]
      )


-- Basic parsers--

item :: Parser Char
item =
    P (\input -> case input of
        [] -> []
        (x : xs) -> [(x, xs)])

sat :: (Char -> Bool) -> Parser Char
sat p =
    do
      x <- item
      if p x then return x else empty -- return item defined above

digits :: [Char]
digits = ['0' .. '9']

isDigit :: Char -> Bool
isDigit x = elem x digits

digitCase :: Parser Char
digitCase = sat isDigit

lowers :: [Char]
lowers = ['a' .. 'z']

isLower :: Char -> Bool
isLower x = elem x lowers

lowerCase :: Parser Char
lowerCase = sat isLower

uppers :: [Char]
uppers = ['A' .. 'Z']

isUpper :: Char -> Bool
isUpper x = elem x uppers

upperCase :: Parser Char
upperCase = sat isUpper

isLetter :: Char -> Bool
isLetter x = isUpper x || isLower x

letterCase :: Parser Char
letterCase = sat isLetter

isAlphaNum :: Char -> Bool
isAlphaNum x = isLetter x || isDigit x

alphaNumCase :: Parser Char
alphaNumCase = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String 
string [] = return []
string (x : xs) = 
    do 
        char x
        string xs 
        return (x : xs)

anIdentifier :: Parser String  
anIdentifier = 
    do 
        x <- letterCase
        xs <- many alphaNumCase    -- Many takes from 0 to n
        return (x : xs)


-- Integer positive number
natInt :: Parser Int        
natInt =                   
    do 
        xs <- some digitCase
        return (read xs)

--Integer positive and negative numbers

int :: Parser Int                  
int = 
    do
    char '-' 
    n <- natInt
    return (-n)
    <|> natInt             


spaces :: [Char]
spaces = ['\n', '\t', '\r', ' ']

isSpace :: Char -> Bool
isSpace x = elem x spaces

aSpace :: Parser ()               
aSpace = do 
            many (sat isSpace)        
            return ()


token :: Parser a -> Parser a         -- deletes spaces
token p =
    do 
        aSpace
        v <- p
        aSpace
        return v


identifier :: Parser String           
identifier = token anIdentifier

naturalNumber :: Parser Int           
naturalNumber = token natInt

integer :: Parser Int                 
integer = token int

symbol :: String -> Parser String     
symbol xs = token (string xs)

-- ARITHMETIC EVALUATION --

aExp  :: Parser ArithExpr         
aExp = do chain aTerm op
    where                        
      op = 
          do
            symbol "+";
            return Add
          <|>
          do
            symbol "-";
            return Sub

aTerm :: Parser ArithExpr       
aTerm = do chain aFactor op     
    where 
      op = 
        do
          symbol "*";
          return Mul
        <|>
        do
          symbol "/";
          return Div
        <|>
        do
          symbol "^";
          return Power


aFactor :: Parser ArithExpr
aFactor = do (Constant <$> integer)
          <|>
          do
            i <- identifier
            return (ArithVariable i)
          <|>
          do
            symbol "("
            a <- aExp
            symbol ")"
            return a

bExp :: Parser BoolExpr          
bExp = chain bTerm op            
  where op = do                  
            symbol "Or"
            return Or

bTerm :: Parser BoolExpr        
bTerm = chain bFact op        
  where op = do
            symbol "And"
            return And

bFact :: Parser BoolExpr
bFact =
  do
    symbol "True"
    return (Boolean True)
    <|> do
      symbol "False"
      return (Boolean False)
    <|> do
      symbol "Not"
      Not <$> bExp
    <|> do
      symbol "("
      b <- bExp
      symbol ")"
      return b
    <|> do 
        a1 <- aExp 
        do
            symbol "<"
            a2 <- aExp 
            return (Lt a1 a2)
            <|> do
              symbol ">"
              a2 <- aExp 
              return (Gt a1 a2)
            <|> do
              symbol "<="
              a2 <- aExp 
              return (Lte a1 a2)
            <|> do
              symbol ">="
              a2 <- aExp 
              return (Gte a1 a2)
            <|> do
              symbol "=="
              a2 <- aExp 
              return (Eq a1 a2)
            <|> do
              symbol "!="
              a2 <- aExp 
              return (Neq a1 a2)
    <|> (BoolVariable <$> identifier) 


command :: Parser Command
command =
  arithDeclare
    <|> boolDeclare 
    <|> arithAssign
    <|> boolAssign 
    <|> ifElse
    <|> whiledo
    <|> skip 

program :: Parser [Command]         
program = do many command

arithDeclare :: Parser Command
arithDeclare =
  do
    symbol "int"              -- int id = 4;
    i <- identifier
    symbol "="
    r <- ArithDeclare i <$> aExp      
    symbol ";"
    return r

boolDeclare :: Parser Command
boolDeclare =
  do
    symbol "bool"             -- bool id=True;
    i <- identifier
    symbol "="
    r <- BoolDeclare i <$> bExp
    symbol ";"
    return r


arithAssign :: Parser Command
arithAssign =
  do
    i <- identifier
    symbol "="
    r <- ArithAssign i <$> aExp 
    symbol ";"
    return r
    
boolAssign  :: Parser Command
boolAssign  =
  do
    i <- identifier
    symbol "="
    r <- BoolAssign  i <$> bExp
    symbol ";"
    return r

skip  :: Parser Command
skip  =
  do
    symbol "skip"
    symbol ";"
    return Skip 

ifElse :: Parser Command
ifElse =
  do
    symbol "if"
    symbol "("
    b <- bExp
    symbol ")"
    symbol "{"
    thenP <- program
    symbol "}"
    do
      symbol "else"
      symbol "{"
      elseP <- program
      symbol "}"
      return (IfElse b thenP elseP)
      <|> do
        return (IfElse b thenP [Skip])

whiledo :: Parser Command
whiledo =
  do
    symbol "whiledo"
    symbol "("
    b <- bExp
    symbol ")"
    symbol "{"
    p <- program
    symbol "}"
    return (Whiledo b p)


-- MAIN PARSE FUNCTIONS

parse :: String -> ([Command], String)
parse s = case p s of
  [] -> ([], "")
  [(c, s)] -> (c, s)
  where
    (P p) = program


parseFailed :: ([Command], String) -> Bool
parseFailed (_, "") = False
parseFailed (_, _) = True

getParsedCommands :: ([Command], String) -> [Command]
getParsedCommands (c, _) = c

getRemainingInput :: ([Command], String) -> String
getRemainingInput (_, s) = s