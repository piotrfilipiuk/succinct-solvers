{
module LexerStar where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
@id    = [A-Za-z0-9][A-Za-z0-9'_@]*

tokens :-
       $white	;
       "("	{ \s -> LPAREN }
       ")" 	{ \s -> RPAREN }
       "["	{ \s -> LBRACK }
       "]"	{ \s -> RBRACK }
       "," 	{ \s -> COMMA }
       "." 	{ \s -> DOT }
       ";"	{ \s -> SEMI }
       "!" 	{ \s -> NOT }
       "=>" 	{ \s -> IMPLY } 
       "&" 	{ \s -> AND }
       "|" 	{ \s -> OR }
       "A" 	{ \s -> FORALL } 
       "E" 	{ \s -> EXISTS }
       "true" 	{ \s -> TRUE }
       "=" 	{ \s -> LexerStar.EQ }
       "!=" 	{ \s -> LexerStar.NEQ }
       @id	{ \s -> ID s }

{
data Token =
     LPAREN
     | RPAREN
     | LBRACK
     | RBRACK
     | COMMA
     | DOT
     | SEMI
     | NOT
     | IMPLY
     | AND
     | OR
     | FORALL
     | EXISTS
     | TRUE
     | EQ
     | NEQ
     | ID String
     deriving (Eq, Show)
}