module Solver where

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import List(nub,concat)
import Data.Maybe(fromJust)
import Data
import Env
--import Result
--import Infl
import DiffUtils
import Parser
import Lexer

createData :: FilePath -> IO (String, Data.Clause, [Int])
createData filePath = do
  content <- readFile filePath
  let solverData = toSolverData . formula . alexScanTokens $ content
      univ = Map.elems (Data.atoms solverData)
  return (filePath, cls solverData, univ)  

solveFromFile filePath = do
  content <- readFile filePath
  return . solveFromString $ content

solveFromString :: String -> Map.Map String [[String]]
solveFromString s = getResult res solverData
                    where solverData = toSolverData . formula . alexScanTokens $ s
                          --universe = Map.elems (Data.atoms solverData)
                          res = solve solverData

parseFromString s = (cls solverData)
    where solverData = toSolverData . formula . alexScanTokens $ s

getResult ss sd = 
          Map.mapWithKey mapPredicate (predicates sd)
          where swappedAtoms = Map.foldrWithKey 
                               (\k a acc -> IntMap.insert a k acc) 
                               (IntMap.empty) (atoms sd)
                mapTuple xs = map (\x -> swappedAtoms IntMap.! x) xs
                mapPredicate k pd = map mapTuple . getTuples (pid pd) $ ss

trieToTuples res solverData = Map.fold f IntMap.empty (Data.predicates solverData)
             where f pd acc = IntMap.insert (pid pd) (getTuples (pid pd) res) acc

solve solverData = result ss where
  univ = Map.elems (Data.atoms solverData)
  iss = DiffSSt { result = IntMap.empty, infl = IntMap.empty, memTab = IntMap.empty }
  ss = execute (cls solverData) $ (IntMap.empty, iss)
  check (Query p args) cont (env, ss) = 
      foldr f ss' tuples
      where f tuple ssf = case unify suffix tuple env of
      	      Just env' -> cont (env', ssf)
	      Nothing -> ssf
	    (prefix, suffix) = splitArguments args env
	    tuples = getTuplesWithPrefix p prefix ss
	    ss' = if (not $ null suffix) || null tuples 
	      	      	 then register p prefix f ss 
	      		 else ss

  check (NegQuery p args) cont (env, ss) =
      foldr f ss envs
      where vars = nub . getNotEvaluatedVariables args $ env
      	    envs = createEnvList vars univ env
	    f e ssf = if contains p tuple ssf
	      	      then ssf
		      else cont (e, ssf)
		      where tuple = evaluate args e

{-
  check (NegQuery p args) cont (env, result, infl, memTable) =
      foldr f (result, infl, memTable) tpls -- envs
      where vars = nub . getNotEvaluatedVariables args $ env --contains duplicates FIX IT!!
--      	    envs = createEnvList vars univ env
            tpls = unifiable vars args univ env
	    f tpl (r, i, mt) = if contains p tpl r
	      	  then (r, i, mt)
		  else cont (env', r, i, mt)
		  where env' = fromJust . unify args tpl $ env
                        --tuple = evaluate args e
-}
{-
  check (NegQuery p args) cont (env, result, infl, memTable) =
        let vars = nub . getNotEvaluatedVariables args $ env
            f x e = map (\a -> IntMap.insert x (Just a) e) univ
            doit ([], es) = es
            doit (x:xs, es) = doit (xs, List.concat (map (f x) es)) 
            checkFail e (r, i, mt) = if contains p (Env.evaluate args e) r
			             then (r, i, mt)
                                     else cont (e, r, i, mt)
            envList = doit (vars, [env])
        in foldr checkFail (result, infl, memTable) envList
-}
  check (AndPre pre1 pre2) cont (env, ss) =
      check pre1 cont' (env, ss)
      where cont' = check pre2 cont

  check (Or pre1 pre2 mem) cont (env, ss) = 
    check pre2 memCont (env, ss')
    where memCont (envf, ssf) = 
                  case tryMemoize mem envf ssf of
                       Just ss' -> cont (envf, ssf)
                       Nothing  -> ssf
          ss' = check pre1 memCont (env, ss)

  check (Exist var pre mem) cont (env, ss) =
    check pre memCont (IntMap.insert var Nothing env, ss)
    where memCont (envf, ssf) = 
                  case tryMemoize mem envf' ssf of
                       Just ssf' -> cont (envf', ssf')
                       Nothing  -> ssf
                  where envf' = IntMap.delete var envf

  check (ForallPre var pre) cont (env, ss) =
    f univ (env, ss)
    where f (hd:tl) (envf, ssf) = let envf' = IntMap.insert var (Just hd) envf
                                  in check pre (f tl) (envf', ssf)
          f _ (envf, ssf) = cont (IntMap.delete var envf, ssf)

  check (Eq (Const c1) (Const c2)) cont (env, ss)
  	| c1 == c2 = cont (env, ss)
	| otherwise = ss

  check (Eq (Const c) (Var x)) cont (env, ss) = 
    case env IntMap.! x of
  	 Just v | v == c -> cont (env, ss)
	        | otherwise -> ss
         Nothing -> cont (env', ss)
		    where env' = IntMap.insert x (Just c) env

  check (Eq (Var x) (Const c)) cont (env, ss) =
        check (Eq (Const c) (Var x)) cont (env, ss)

  check (Eq (Var x1) (Var x2)) cont (env, ss) =
        case (m1, m2) of
             (Just v1, Just v2) | v1 == v2 -> cont (env, ss)
                                | otherwise -> ss
             (Just v1, Nothing) -> let env' = IntMap.insert x2 m1 env
                                   in cont (env', ss)
             (Nothing, Just v2) -> let env' = IntMap.insert x1 m2 env
                                   in cont (env', ss)
             (Nothing, Nothing) -> foldr f ss univ
        where m1 = env IntMap.! x1
	      m2 = env IntMap.! x2
              f atom ssf = cont (envf, ssf)
	      	where envf = IntMap.insert x1 (Just atom) . IntMap.insert x2 (Just atom) $ env

  check (Neq (Const c1) (Const c2)) cont (env, ss)
  	| c1 /= c2 = cont (env, ss)
	| otherwise = ss

  check (Neq (Const c) (Var x)) cont (env, ss) =
    case env IntMap.! x of
  	Just v | v /= c -> cont (env, ss)
	       | otherwise -> ss
	Nothing -> foldr f ss atoms
		where atoms = filter (/=c) univ
		      f atom ssf = cont (env', ssf)
			where env' = IntMap.insert x (Just atom) env

  check (Neq (Var x) (Const c)) cont (env, ss) = 
        check (Neq (Const c) (Var x)) cont (env, ss)

  check (Neq (Var x1) (Var x2)) cont (env, ss) =
    case (v1, v2) of
  	 (Just a1, Just a2) | a1 /= a2 -> cont (env, ss)
	                    | otherwise -> ss
	 (Just a, Nothing) -> foldr (f x2 a) ss univ
	 (Nothing, Just a) -> foldr (f x1 a) ss univ
	 (Nothing, Nothing) -> foldr g ss distinct
	 where f x c atom ssf
                 | atom /= c = cont (envf, ssf)
                 | otherwise = ssf
                 where envf = IntMap.insert x (Just atom) env
               g (c1, c2) ssg = cont (envg, ssg)
                 where envg = IntMap.insert x1 (Just c1) . IntMap.insert x2 (Just c2) $ env
               distinct = [(c1,c2) | c1 <- univ, c2 <- univ, c1/=c2]
               v1 = env IntMap.! x1
               v2 = env IntMap.! x2
  check TruePre cont (env, ss) =  cont (env, ss)
  check FalsePre cont (env, ss) = ss

  execute (Assertion p args) (env, ss) =
    foldr f ss tuples
    where vars = nub . getNotEvaluatedVariables args $ env
          tuples = nub . unifiable vars args univ $ env
          f tuple ssf = case tryInsert p tuple ssf of
                             Just ssf' -> resume p tuple ssf'
                             _ -> ssf
{-          f tuple (r, i, mt)
            | r /= r' = resume p tuple r' i mt
            | otherwise = (r, i, mt)
            where r' = insert p tuple r -}

  execute TT (env, ss) = ss

  execute (And cl1 cl2) (env, ss) =
    execute cl2 (env, ss')
    where ss' = execute cl1 (env, ss)

  execute (Imply pre cl) (env, ss) =
    check pre cont' (env,ss)
    where cont' = execute cl

  execute (Forall var cl) (env, ss) =
    execute cl (IntMap.insert var Nothing env, ss)