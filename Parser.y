{
module Parser where
import Lexer
import Data
}

%name formula
%tokentype { Token }
%error { parseError }

%token 
  id { ID $$ }
  and { AND } 
  or { OR }
  lparen { LPAREN }
  rparen { RPAREN }
  dot { DOT }
  comma { COMMA }
  not { NOT }
  imply { IMPLY }
  forall { FORALL }
  exists { EXISTS }
  tt { TRUE }
  ff { FALSE}
  eq { Lexer.EQ }
  neq { NEQ }

%left imply
%left forall exists
%left or
%left and
%left eq neq
%left not

%%

Formula
        : lparen Formula rparen { $2 }
	| tt { Data.True }
        | ff { Data.False }
	| id eq id { Equal $1 $3 }
	| id neq id { Nequal $1 $3 }
	| Formula and Formula { Wedge $1 $3 }
	| Formula imply Formula { Rightarrow $1 $3 }
	| Formula or Formula { Vee $1 $3 }
        | id lparen VarList rparen { Predicate $1 (reverse $3) }
        | not id lparen VarList rparen { NegPredicate $2 (reverse $4) }
	| exists id dot Formula { E $2 $4 }
	| forall id dot Formula { A $2 $4 }

VarList
        : id { [$1] }
	| VarList comma id { $3 : $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
