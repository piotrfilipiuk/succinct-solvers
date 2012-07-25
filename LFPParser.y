{
module LFPParser where
import LFPLexer
import Data
import LFPLogic
}

%name layerFormula
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
  ff { FALSE }
  constrain { CONSTRAIN }
  define { DEFINE }
  eq { LFPLexer.EQ }
  neq { NEQ }

%left imply
%left forall exists
%left or
%left and
%left eq neq
%left not

%%

LFPClauses : ClauseList { reverse $1 }

ClauseList
: Layer { [$1] }
| ClauseList comma Layer { $3 : $1 }

Layer
: define lparen Formula rparen { DefFormula $3 }
| constrain lparen Formula rparen { ConFormula $3 }

Formula
: exists id dot Formula { E $2 $4 }
| forall id dot Formula { A $2 $4 }
| F2 { $1 }

F2
: F2 imply F3 { Rightarrow $1 $3 }
| F3 { $1 }

F3
: F3 or F4 { Vee $1 $3 }
| F4 { $1 }

F4
: F4 and F5 { Wedge $1 $3 }
| F5 { $1 }

F5
: id lparen VarList rparen { Predicate $1 (reverse $3) }
| not id lparen VarList rparen { NegPredicate $2 (reverse $4) }
| tt { Data.True }
| ff { Data.False }
| id eq id { Equal $1 $3 }
| id neq id { Nequal $1 $3 }
| lparen Formula rparen { $2 }

VarList
: id { [$1] }
| VarList comma id { $3 : $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
