module SolverStar where

import qualified Data.IntMap as IntMap(empty,insert,delete,(!))
import qualified Data.Map as Map(elems,foldrWithKey,empty)
import qualified Data.IntSet as IntSet(fromList,IntSet)
import Debug.Trace(trace)
import Control.DeepSeq

import DataStar
import EnvStar
import UtilsStar
import OutputStar(printSolution)
import ParserStar(formula)
import LexerStar(alexScanTokens)
import ConstantPropagation

import WhileLang
import qualified WhileLexer as WL(alexScanTokens)
import qualified WhileParser as WP(program)

parseFromString = createClause . formula . alexScanTokens

printClause = cls . createClause . formula . alexScanTokens

--cl = "R(a,b;) & (A x. A y. R(x,y;) => S(x,y;))"
--cl = "R(a,b;) & (A x. A y. R(x,y;) => S(y,x;))"
--cl = "R(a,b;) & (A x. A y. !R(x,y;) => S(x,y;))"
--cl = "R(a,b;) & Q(a;) & (A x. A y. (Q(x;) & !R(x,y;) => S(x,y;)))"
--cl = "R(a,b;) & Q(c,a;) & (A x. A y. (Q(x,y;) | R(x,y;) => S(x,y;)))"
--cl = "R(a;[b]) & R(b;[c]) & Q(a;) & (A x. A z. A y. (R(x;y) & y(z) => S(x,z;)))"
--cl = "R(a,b;[b]) & R(a,b;f([b],[c],[c]))"
--cl = "R(g(a,c),b;[a]) & R(c,d;[e])"

getPG s = createPG stmt'
      where stmt = WP.program . WL.alexScanTokens $ s
            (stmt', vars) = transStmt stmt Map.empty

parseWhileProg s = WP.program . WL.alexScanTokens $ s

prog = "x:=5; y:=7; while x = 10 do x:=x+1 od; x:=33"
--prog = "x:=5;x:=x+7;y:=x"
doCP s = printSolution ss solverData
  where solverData = generateCP . WP.program . WL.alexScanTokens $ s
        universe = Map.elems (atoms solverData)
        ss = solve (cls solverData) universe topL complL beta'
        topL = top IntMap.empty
        complL = compl IntMap.empty
        beta' x = error "beta' for CP should not be used."

cp_cls = "(A s. CP(q;f12(s))) & (A s. CP(q;s) => CP(t;f32(s)))"

cp_test_cls = "(A x. CP(s1;init(x))) & (A x. CP(s1;x) => CP(s2;f12(x))) & (A x. CP(s2;x) => CP(s3;f23(x))) & (A x. CP(s2;x) => CP(s4;f24(x))) & (A x. CP(s3;x) => CP(s2;f32(x)))"

{-
solveFromString s = printSolution ss solverData
            where solverData = parseFromString s :: SolverData Interval --(IntSet.IntSet)
                  universe = Map.elems (atoms solverData)
                  ss = solve (cls solverData) universe topL complL beta'
                  preds = map (\ (p,_) -> p) . Map.elems . predicates $ solverData
                  idToNames = Map.foldrWithKey (\k v acc -> IntMap.insert v k acc) IntMap.empty (atoms solverData)
                  topL = top EmptyInterval
                  complL = compl EmptyInterval
                  beta' x = beta str
                          where str = idToNames IntMap.! x
-}
solveCP s = printSolution ss solverData
            where solverData = parseFromString s :: SolverData StateCP
                  universe = Map.elems (atoms solverData)
                  ss = solve (cls solverData) universe topL complL beta'
                  preds = map (\ (p,_) -> p) . Map.elems . predicates $ solverData
                  --idToNames = Map.foldrWithKey (\k v acc -> IntMap.insert v k acc) IntMap.empty (atoms solverData)
                  topL = top IntMap.empty
                  complL = compl IntMap.empty
                  beta' x = error "beta' for CP should not be used."
{-
solveFromFile filePath = do
              contents <- readFile filePath
              solveFromString contents
-}
--solve :: (Lattice a, Analysis a, Ord a, Eq a) => Clause a -> [Int] -> SolverState a
solve cl univ topL complL beta' = finalState where
  initialState = SolverState { getResult=IntMap.empty
                             , getInfl=IntMap.empty
                             , getMemTab=IntMap.empty }
  finalState = execute cl (IntMap.empty, initialState)
--  all = IntSet.fromList $ univ
--  topL = top all
--  complL = compl all
  check (Query p args) _ _ | trace (show args) False = undefined
  check (Query p args) cont (env, ss) = 
      foldr f ss' tuples
      where f tuple st = foldl (\st' env' -> cont (env', st')) st envs
                         where envs = unify beta' suffix tuple env
	    (prefix, suffix) = splitArgs args env
	    tuples = getTuplesWithPrefix p prefix ss
	    ss' = if (isNotEmptyArgs suffix) || null tuples 
	      	      	 then register p prefix f ss
	      		 else ss

  check (NegQuery p args) cont (env, ss) =
      foldr f ss envs
      where vars = getNotEvaluatedVariables args env
      	    envs = createEnvList vars univ topL env
            f _env _ss = case createEnv p tuple args beta' complL _env _ss of
                            Just _env' -> cont (_env', _ss)
                            Nothing -> _ss
                       where tuple = evaluate beta' args _env
  
  check (Inc y (VarU x)) cont (env, ss) =
        case env IntMap.! x of
             Just (Atom a) | beta' a `leq` l -> cont (env',ss)
                           | otherwise -> ss
             Nothing -> foldr f ss univ
        where l = case env IntMap.! y of 
                       Just (LElem v) -> v
                       Nothing -> topL
              env' = IntMap.insert y (Just . LElem $ l) env
              f a acc = if beta' a `leq` l
                        then cont (IntMap.insert x (Just . Atom $ a) env', acc)
                        else acc

  check (Inc y (ConstU a)) cont (env, ss) =
        if beta' a `leq` l
        then cont (env', ss)
        else ss
        where l = case env IntMap.! y of 
                       Just (LElem v) -> v
                       Nothing -> topL
              env' = IntMap.insert y (Just . LElem $ l) env

  check (Inc y (FunU f xs)) cont (env, ss) = undefined

  check (AndPre pre1 pre2) cont (env, ss) =
      check pre1 cont' (env, ss)
      where cont' = check pre2 cont

  check (Or pre1 pre2 mem) cont (env, ss) = 
{-        check pre2 cont (env, ss')
        where ss' = check pre1 cont (env, ss) -}
    check pre2 memoize (env, ss')
        where memoize (_env, _ss) = 
                case tryMemoize mem _env _ss of
                  Just _ss' -> cont (_env,_ss')
                  Nothing   -> _ss
              ss' = check pre1 memoize (env,ss)

  check (Exist var pre mem) cont (env, ss) =
    check pre memoize (IntMap.insert var Nothing env, ss)
          where memoize (_env, _ss) = 
                  case tryMemoize mem (IntMap.delete var _env) _ss of
                    Just _ss' -> cont (_env, _ss')
                    Nothing  -> _ss

  execute (Assertion p args) (env, ss) =
    foldr f ss tuples
    where vars = getNotEvaluatedVariables args $ env
          tuples = unifiable vars args univ beta' topL $ env
          --f tuple _ | trace "xs" False = undefined
          f tuple ss = case tryInsert p tuple ss of
            Just ss' -> resume p tuple ss'
            _ -> ss

  execute TT (env, ss) = ss

  execute (And cl1 cl2) (env, ss) =
    execute cl2 (env, ss')
    where ss' = execute cl1 (env, ss)

  execute (Imply pre cl) (env, ss) =
    check pre cont' (env, ss)
    where cont' = execute cl

  execute (Forall var cl) (env, ss) =
    execute cl (IntMap.insert var Nothing env, ss)