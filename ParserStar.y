{
module ParserStar where
import LexerStar
import DataStar
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
  lbrack { LBRACK }
  rbrack { RBRACK }
  dot { DOT }
  comma { COMMA }
  semi { SEMI }
  not { NOT }
  imply { IMPLY }
  forall { FORALL }
  exists { EXISTS }
  tt { TRUE }
  eq { LexerStar.EQ }
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
	| tt { TrueFormula }
	| id eq id { Equal $1 $3 }
	| id neq id { Nequal $1 $3 }
	| Formula and Formula { Wedge $1 $3 }
	| Formula imply Formula { Rightarrow $1 $3 }
	| Formula or Formula { Vee $1 $3 }
        | id lparen ArgumentFU rparen { Inclusion $1 $3 }
        | id lparen ArgList rparen { Predicate $1 $3 }
        | not id lparen ArgList rparen { NegPredicate $2 $4 }
	| exists id dot Formula { E $2 $4 }
	| forall id dot Formula { A $2 $4 }

ArgList 
: LeftList semi { LeftF (reverse $1) }
| LeftList semi ArgumentFL { BothF (reverse $1) $3 }
| semi ArgumentFL { RightF $2 }

LeftList
        : ArgumentFU { [$1] }
	| LeftList comma ArgumentFU { $3 : $1 }

RightList
: ArgumentFL { [$1] }
| RightList comma ArgumentFL { $3 : $1 }

ArgumentFU
: id { SimpleFU $1 }
| id lparen LeftList rparen { FunFU $1 (reverse $3) }

ArgumentFL
: lbrack ArgumentFU rbrack { AbsFL $2 }
| id { VarFL $1 }
| id lparen RightList rparen { FunFL $1 (reverse $3) }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
