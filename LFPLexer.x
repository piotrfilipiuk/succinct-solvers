{
module LFPLexer where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
@id    = [A-Za-z0-9][A-Za-z0-9'_@]*

tokens :-
       $white	;
       "define" { \s -> DEFINE }
       "constrain" { \s -> CONSTRAIN }
       "("	{ \s -> LPAREN }
       ")" 	{ \s -> RPAREN }
       "," 	{ \s -> COMMA }
       "." 	{ \s -> DOT }
       "!" 	{ \s -> NOT }
       "=>" 	{ \s -> IMPLY } 
       "&" 	{ \s -> AND }
       "|" 	{ \s -> OR }
       "A" 	{ \s -> FORALL } 
       "E" 	{ \s -> EXISTS }
       "true" 	{ \s -> TRUE }
       "false"  { \s -> FALSE }
       "=" 	{ \s -> LFPLexer.EQ }
       "!=" 	{ \s -> LFPLexer.NEQ }
       @id	{ \s -> ID s }

{
data Token =
     LPAREN
     | RPAREN
     | COMMA
     | DOT
     | NOT
     | IMPLY
     | AND
     | OR
     | FORALL
     | EXISTS
     | TRUE
     | FALSE
     | EQ
     | NEQ
     | DEFINE
     | CONSTRAIN
     | ID String
     deriving (Eq, Show)
}