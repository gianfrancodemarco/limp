GRAMMAR:
program ::=   <command> | <command> <program>

command ::=   <skip> ";"
          |   <ifElse> ";"
		  |   <while> ";"
          |   <boolDeclare> ";"
		  |   <arithDeclare> ";"
          |   <boolAssign> ";"
          |   <arithAssign> ";"
          |   <arrayDeclare> ";"
          |   <arrayAssign> ";"
          |   <arrayFullAssign> ";"
          |   <stackDeclare> ";"
          |   <stackPush> ";"
          |   <stackPop> ";"
          |   <queueDeclare> ";"
          |   <queueEnqueue> ";"
          |   <queueDequeue> ";"
		  
		  
skip ::=   "skip"


ifElse ::=    "if" "("<bExp>")" "{" <program> "}"
            | "if" "("<bExp>")" "{" <program>"}" "else" "{" <program> "}"

while ::=      "while" "("<bExp>")" "{" <program> "}"

______________________________________________________________________________________________


boolDeclare ::=   "bool" <identifier> "=" <bExp> ";"

bExp ::= <bTerm> ["Or" <bTerm>]*

bTerm ::=  <bFact> ["And" <bFact>]*

bFact ::=  True
	| False 
	| "Not" <bExp>
	| "(" <bExp> ")" 
	| "empty" <identifier> -- stacks
	| "qempty" <identifier> -- queue 
	| "bool(" <identifier> ")" -- this is needed to distinguish from [line 67]; also bool(s) needs to be declared before arith(s)
	| <aExp> < <aExp> 
	| <aExp> > <aExp> 
	| <aExp> <= <aExp> 
	| <aExp> >= <aExp> 
    | <aExp> == <aExp> 
	| <aExp> != <aExp>

______________________________________________________________________________________________

arithDeclare ::=   "int" <identifier> "=" <aExp> ";"

identifier ::=    [a-zA-Z_][a-zA-Z_0-9]*

aExp ::=    <aTerm> [{"+"|"-"} <aTerm>]*

aTerm ::=   <aFactor> [{"*"|"/"|"^"} <aFactor>]*

aFactor ::= "top" <identifier>
		| "first" <identifier>
		| "length" <identifier>
		| <identifier> "[" <aExp> "]"
		| <identifier>
		| <integer> | "(" <aExp> ")"
        
		
integer ::= - <natInt> | <natInt>
		
natInt ::= <digit> <natInt> | <digit>

digit := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

______________________________________________________________________________________________


arithAssign ::=    <identifier> "=" <aExp> ";"

boolAssign ::=    <identifier> "=" <bExp> ";"

______________________________________________________________________________________________

arrayDeclare ::= "array" <identifier> "["<aExp>"]"
		| "array" <identifier> "=" <arrayFull>
		| "array" <identifier> "=" <arrayScalarProduct>
		| "array" <identifier> "=" <arrayConcat>
		| "array" <identifier> "=" <arrayDotProduct>
		
arrayFull ::= "[" <aExp> ["," <aExp>]* "]"

arrayScalarProduct ::= "scalar" <identifier> <aExp>

arrayConcat ::= "concat" <identifier> <identifier>
		| <identifier> "++" <identifier>
		
arrayDotProduct ::= "dot" <identifier> <identifier>

arrayAssign ::= <identifier> "[" <aExp> "]" "=" <aExp>

arrayFullAssign ::= <identifier> "=" <arrayFull>
		| <identifier> "=" <arrayScalarProduct>
		| <identifier> "=" <arrayConcat>
		| <identifier> "=" <arrayDotProduct>

______________________________________________________________________________________________

stackDeclare ::= "stack" <identifier>

stackPush ::= "push" <identifier> <aExp>

stackPop ::= "pop" <identifier>

______________________________________________________________________________________________

queueDeclare ::= "queue" <identifier>

queueEnqueue ::= "enqueue" <identifier> <aExp>

queueDeclare ::= "dequeue" <identifier>

______________________________________________________________________________________________






