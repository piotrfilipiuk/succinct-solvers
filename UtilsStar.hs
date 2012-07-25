module UtilsStar where

import qualified Data.IntMap as IntMap(lookup,insert)
import qualified Data.Set as Set(member,insert,singleton)

import ResultStar
import InflStar
import EnvStar
import DataStar

---
--- API for Result module.
---

--tryInsert :: Lattice a => Int -> Tuple a -> SolverState a -> Maybe (SolverState a)
tryInsert p tuple ss = 
          case ResultStar.tryInsert p tuple (getResult ss) of
               Just result -> Just $ ss { getResult = result }
               Nothing -> Nothing

--contains :: Lattice a => Int -> Tuple a -> SolverState a -> Bool
--contains = undefined

--createEnv :: (Lattice a, Analysis a) => Int -> Tuple a -> Args a -> Env a -> SolverState a -> Maybe (Env a)
createEnv pred tuple args beta' complL env ss = ResultStar.createEnv pred tuple args beta' complL env $ getResult ss

--getTuplesWithPrefix :: Lattice a => Int -> Tuple a -> SolverState a -> [Tuple a]
getTuplesWithPrefix p tuple ss = ResultStar.getTuplesWithPrefix p tuple (getResult ss)

tuples :: Lattice a => Int -> SolverState a -> [Tuple a]
tuples p ss = getTuples p (getResult ss)

---
--- API for Infl module.
---

--register :: Lattice a => Int -> Tuple a -> Consumer a -> SolverState a -> SolverState a
register p tuple f ss = ss { getInfl = result }
                        where result = InflStar.register p tuple f (getInfl ss)

resume :: Int -> Tuple a -> SolverState a -> SolverState a
resume = InflStar.resume

---
--- Memoization
---

tryMemoize :: (Lattice a, Eq a, Ord a) => Int -> Env a -> SolverState a -> Maybe (SolverState a)
tryMemoize key env ss = 
           case IntMap.lookup key memTab of
                Just xs -> case Set.member env xs of
                                True -> Nothing
                                False -> Just ss'
                           where memTab' = IntMap.insert key (Set.insert env xs) memTab
                                 ss' = ss { getMemTab = memTab' }
                Nothing -> Just $ ss'
                           where memTab' = IntMap.insert key (Set.singleton env) memTab
                                 ss' = ss { getMemTab = memTab' }
           where memTab = (getMemTab ss)