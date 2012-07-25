{
module WhileParser where
import Prelude hiding (EQ)
import WhileLang
import WhileLexer
}

%name program
%tokentype { Token }
%error { parseError }

%token
id { ID $$ }
num { NUM $$ }
while { WHILE }
do { DO }
od { OD }
if { IF }
fi { FI }
then { THEN }
else { ELSE }
asgn { ASGN }
true { TRUE }
false { FALSE }
eq { EQ }
neq { NEQ }
not { NOT }
lparen { LPAREN }
rparen { RPAREN }
semi { SEMI }
plus { PLUS }
minus { MINUS }
mul { MUL}
div { DIV }
skip { SKIP }

%left eq neq
%left plus minus
%left mul div

%%

Statement
: Statement semi Statement { Semi $1 $3 }
| skip { Skip }
| id asgn IntExpression { Asgn $1 $3 }
| if BoolExpression then Statement else Statement fi { If $2 $4 $6 }
| while BoolExpression do Statement od { While $2 $4 }

BoolExpression
: lparen BoolExpression rparen { $2 }
| not BoolExpression { Not $2 }
| IntExpression eq IntExpression { Eq $1 $3 }
| IntExpression neq IntExpression { Neq $1 $3 }
| true { B True }
| false { B False }

IntExpression
: lparen IntExpression rparen { $2 }
| id { V $1 }
| num { I $1 }
| IntExpression plus IntExpression { Add $1 $3 }
| IntExpression minus IntExpression { Sub $1 $3 }
| IntExpression mul IntExpression { Mul $1 $3 }
| IntExpression div IntExpression { Div $1 $3 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
