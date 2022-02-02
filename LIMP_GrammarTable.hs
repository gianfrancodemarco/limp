GRAMMAR:
program ::=   <command> | <command> <program>

command ::=   <arithDeclare> ";"
          |   <boolDeclare> ";"
          |   <arithAssign> ";"
          |   <boolAssign> ";"
          |   <arrayDeclare> ";"
          |   <arrayAssign> ";"
          |   <ifElse> ";"
          |   <whiledo> ";"
          |   <skip> ";"

identifier ::=    [a-zA-Z_][a-zA-Z_0-9]*
digit := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

arithDeclare ::=   "int" <identifier> "=" <aExp> ";"
aExp ::=    <aTerm> [{"+"|"-"} <aTerm>]*
aTerm ::=   <aFactor> [{"*"|"/"|"^"} <aFactor>]*
aFactor ::= <integer> | "(" <aExp> ")"
        | <identifier> "[" <aExp> "]"
integer ::= - <natInt> | <natInt>
natInt ::= <digit> <natInt> | <digit>


boolDeclare ::=   "bool" <identifier> "=" <bExp> ";"
bExp ::=        <bTerm> ["Or" <bTerm>]*
bTerm ::=       <bFact> ["And" <bFact>]*
bFact ::=       True| False | "Not" <bExp> | "(" <bExp> ")" | <aExp> < <aExp> 
                | <aExp> > <aExp> | <aExp> <= <aExp> | <aExp> >= <aExp> 
                | <aExp> == <aExp> | <aExp> != <aExp> | <identifier>


arithAssign ::=    <identifier> "=" <aExp> ";"

boolAssign ::=    <identifier> "=" <bExp> ";"


ifElse ::=    "if" "("<bExp>")" "{" <program> "}"
            | "if" "("<bExp>")" "{" <program>"}" "else" "{" <program> "}"

whiledo ::=      "whiledo" "("<bExp>")" "{" <program> "}"

skip ::=   "skip"










