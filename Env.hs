module Env where

import qualified Data.IntMap as IntMap
import qualified Data.List as List(foldl,foldl',concatMap,replicate,length,zip)
import Data.Maybe
import qualified Control.Exception as Exc
import qualified Data.IntSet as IntSet
import Data

type Env = IntMap.IntMap (Maybe Int)

unify :: [Argument] -> [Int] -> IntMap.IntMap (Maybe Int) -> Maybe (IntMap.IntMap (Maybe Int))
unify [] [] env = Just env
unify (Const a:args) (t:tuple) env
      | a == t = unify args tuple env
      | otherwise = Nothing
unify (Var x:args) (t:tuple) env
      | isNothing value = unify args tuple (IntMap.insert x (Just t) env)
      | (fromJust value) == t = unify args tuple env
      | otherwise = Nothing
      where value = env IntMap.! x
unify _ _ _ = error "Lists should have equal lengths."

--Result contains duplicates, fix that!!
getNotEvaluatedVariables :: [Argument] -> IntMap.IntMap (Maybe Int) -> [Int]
getNotEvaluatedVariables vars env = foldr f [] vars
			 where f (Var x) acc =
			       	 let v = env IntMap.! x
			       	 in if isJust v then acc else x:acc
			       f _ acc = acc

getNotEvaluatedVariables' :: [Argument] -> IntMap.IntMap (Maybe Int) -> [Int]
getNotEvaluatedVariables' vars env = fst . foldr f ([], IntSet.empty) $ vars
			 where {-f (Var x) acc =
			       	 let v = env IntMap.! x
			       	 in if isJust v then acc else x:acc-}
                               f (Var x) (acc, mp)
                                 | IntSet.notMember x mp = case env IntMap.! x of
                                                             Nothing -> (x:acc, newMp)
                                                             _ -> (acc, newMp)
                                 | otherwise = (acc, mp)
                                 where newMp = IntSet.insert x mp 
			       f _ (acc, mp) = (acc, mp)

evaluate args env = foldr f [] args
	 where f (Var x) acc =
	       	 case env IntMap.! x of
		      Just v -> v : acc
		      _ -> error "All variables should be evaluated."
	       f (Const a) acc = a : acc

createEnvList
  :: [IntMap.Key]
     -> [a]
     -> IntMap.IntMap (Maybe a)
     -> [IntMap.IntMap (Maybe a)]
createEnvList vars univ env = List.foldl f [env] vars
	      where f acc var = List.concatMap (g var) acc
                    --f acc var = concat $ map (g var) acc
	      	    g x e = map (\a -> IntMap.insert x (Just a) e) univ

createTuples envList args = foldr f [] envList
	     where f env acc = (evaluate args env) : acc

unifiable :: [Int] -> [Argument] -> [Int] -> (IntMap.IntMap (Maybe Int)) -> [[Int]]
unifiable vars args univ env = createTuples (createEnvList vars univ env) args

splitArguments :: [Argument] -> IntMap.IntMap (Maybe Int) -> ([Int], [Argument])
splitArguments args env = split [] args
	       where split xs [] = (reverse xs, [])
	             split xs ((Const n):tl) = split (n : xs) tl
		     split xs vs@((Var x):tl) = case env IntMap.! x of
		     	 Just value -> split (value : xs) tl
			 Nothing -> (reverse xs, vs)

q :: [Int] -> Int -> [[Int]]
q univ len = List.foldl' f [[]] [0..len-1]
  where f acc _ = concatMap g acc
        g st = map (:st) univ

gg env vars univ = List.foldl' g env zs
   where g en (x,u:us) = IntMap.insert x (Just u) en
         univs = List.replicate (List.length vars) univ
         zs = List.zip vars univs

mkEnvs env vars univ = pp vars univ env
       where aa (v:vs) = pp (v:vs) 
             pp [] _ eps = eps
             pp (v:vs) (u:us) eps = let eps' = IntMap.insert v (Just u) eps
                                    in pp vs univ eps'