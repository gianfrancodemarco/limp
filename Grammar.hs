module Grammar where
import Parser (Parser(..))
import Types (Variable(..))

--------------------------------------------------------------
-- ARTIHMETIC EXPRESSIONS
-- aexp        ::= <aterm> + <aexp> | <aterm> - <aexp> | <aterm>
-- aterm       ::= <afactor> * <aterm> | <afactor>
-- afactor     ::= (<aexp>) | <integer> | <identifier>
--
-- examples:
{-
*Main> parse aexp [] "2"
[([],2,"")]
*Main> parse aexp [] "2+3"
[([],5,"")]
*Main> parse aexp [] "2+3*8"
[([],26,"")]
*Main>
-}
--------------------------------------------------------------

aexp :: Parser Int
aexp = (do t <- aterm
           symbol "+"
           a <- aexp
           return (t+a))
        <|>
       (do t <- aterm
           symbol "-"
           a <- aexp
           return (t-a))
        <|>
       aterm
    {-do {
          t <- aterm;
          do { symbol "+"
             ; e <- aexp
             ; return (t + e); }
           <|>
          do { symbol "-"
             ; e <- aexp
             ; return (t - e); }
           <|>
          return t;
          }-}


aterm :: Parser Int
aterm = do { f <- afactor
           ; symbol "*"
           ; t <- aterm
           ; return (t * f)
            }
            <|>
            afactor

afactor :: Parser Int
afactor = (do symbol "("
              a <- aexp
              symbol ")"
              return a)
            <|>
          (do i <- identifier
              readVariable i)
            <|>
          integer
    {-do {
              symbol "("
            ; e <- aexp
            ; symbol ")"
            ; return e }
           <|>
          do {
              i <- identifier
            ; readVariable i
          }
           <|>
           integer-}

--------------------------------------------------------------
--------------------------------------------------------------
-- BOOLEAN EXPRESSIONS
-- bexp        ::= <bterm> OR <bexp> | <bterm>
-- bterm       ::= <bfactor> AND <bterm> | <bfactor>
-- bfactor     ::= true | false | !<bfactor> | (bexp) | <b
-- bcomparison ::= <aexp> = <aexp> | <aexp> ≤ <aexp>
--------------------------------------------------------------

bexp :: Parser Bool
bexp =  (do b0 <- bterm
            symbol "OR"
            b1 <- bexp
            return (b0 || b1))
        <|>
        bterm

bterm :: Parser Bool
bterm = (do f0 <- bfactor
            symbol "AND"
            f1 <- bterm
            return (f0 && f1))
        <|>
        bfactor

bfactor :: Parser Bool
bfactor = (do symbol "True"
              return True)
          <|>
          (do symbol "False"
              return False)
          <|>
          (do symbol "!"
              b <- bfactor
              return (not b))
          <|>
          (do symbol "("
              b <- bexp
              symbol ")"
              return b)
          <|>
          bcomparison

bcomparison :: Parser Bool
bcomparison = (do a0 <- aexp
                  symbol "="
                  a1 <- aexp
                  return (a0 == a1))
                <|>
              (do a0 <- aexp
                  symbol "<="
                  a1 <- aexp
                  return (a0 <= a1))

--------------------------------------------------------------
--------------------------------------------------------------
-- COMMAND EXPRESSIONS
-- program     ::= <command> | <command> <program>
-- command     ::= <assignment> | <ifThenElse> | <while> | skip;
-- assignment  ::= <identifier> := <aexp>;
-- ifThenElse  ::= if (<bexp>) { <program> } | if (<bexp>) {<program>} else {<program>}
-- while       ::= while (<bexp>) {<program>}
--------------------------------------------------------------

program :: Parser String
program = (do command
              program)
          <|>
          command

command :: Parser String
command = assignment
           <|>
          ifThenElse
           <|>
          while
           <|>
          (do symbol "skip"
              symbol ";")

assignment :: Parser String
assignment = do x <- identifier
                symbol ":="
                v <- aexp
                symbol ";"
                updateEnv Variable{name=x, vtype="", value=v}

ifThenElse :: Parser String
ifThenElse = (do symbol "if"
                 b <- bexp
                 symbol "{"
                 if (b) then
                     (do program
                         symbol "}"
                         (do symbol "else"
                             symbol "{"
                             parseProgram;
                             symbol "}"
                             return "")
                            <|>
                            (return ""))
                 else
                     (do parseProgram
                         symbol "}"
                         (do symbol "else"
                             symbol "{"
                             program
                             symbol "}"
                             return "")
                          <|>
                          return "")
                        )

while :: Parser String
while = do w <- consumeWhile
           repeatWhile w
           symbol "while"
           --symbol "("
           b <- bexp
           --symbol ")"
           symbol "{"
           if (b) then
               (do program
                   symbol "}"
                   repeatWhile w
                   while)
           else
               (do parseProgram
                   symbol "}"
                   return "")

repeatWhile :: String -> Parser String
repeatWhile c = P(\env input -> [(env, "", c ++ input)])
