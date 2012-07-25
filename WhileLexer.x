{
module WhileLexer where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
@id    = [A-Za-z][A-Za-z0-9'_@]*
@number = $digit($digit)*

tokens :-
       $white	;
       "while"	{ \s -> WHILE }
       "do"	{ \s -> DO }
       "od"	{ \s -> OD }
       "if"	{ \s -> IF }
       "fi"	{ \s -> FI }
       "then"	{ \s -> THEN }
       "else"	{ \s -> ELSE }
       ":="	{ \s -> ASGN }
       "true" 	{ \s -> TRUE }
       "false" 	{ \s -> FALSE }
       "skip"	{ \s -> SKIP }
       "=" 	{ \s -> WhileLexer.EQ }
       "!=" 	{ \s -> WhileLexer.NEQ }
       "not"	{ \s -> NOT}
       @id	{ \s -> ID s }
       @number	{ \s -> NUM (read s) }
       "("	{ \s -> LPAREN }
       ")" 	{ \s -> RPAREN }
       ";"	{ \s -> SEMI }
       "+"	{ \s -> PLUS }
       "-"	{ \s -> MINUS }
       "*"	{ \s -> MUL }
       "\\"	{ \s -> DIV }

{
data Token
     = WHILE
     | DO
     | OD
     | IF
     | FI
     | THEN
     | ELSE
     | ASGN
     | TRUE
     | FALSE
     | EQ
     | NEQ
     | NOT
     | ID String
     | NUM Int
     | LPAREN
     | RPAREN
     | SEMI
     | PLUS
     | MINUS
     | MUL
     | DIV
     | SKIP
     deriving(Show,Eq)
}