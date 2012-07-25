{-# Language FlexibleInstances #-}
--module Main(main) where
module ALFPLogic where

import Data
import Data.Ord

import Test.QuickCheck
import qualified Data.IntMap as IntMap(empty,insert,(!),assocs,fromList)
import qualified Data.Map as Map(fromList,insert,empty,fold,elems,size,foldrWithKey)
import qualified Data.List as List(partition,length,foldl',null,maximum,map,maximumBy,reverse,zip,and,foldr)
import Control.Monad(mapM,replicateM,liftM,foldM)
import Debug.Trace(trace)
import Test.QuickCheck.Property (morallyDubiousIOProperty)
import Control.DeepSeq(rnf)
import Control.Exception(bracket_,evaluate)
import qualified Solver as Solver
import qualified BddSolver as BddSolver
import Debug.Trace(trace)
import Fdd
import Bdd
--import SolversTest(runBddSolver,runDiffSolver,mapResult,toStr)
{-
--main = bddcheck
--main = diffcheck
main = qcheck
--main = ptest
{-
main = do
     r <- Solver.solveFromFile "/Users/pifi/Documents/solver1/Application/Examples/neg.cl"
     print r
     return ()
-}
{-
main = do
     r <- BddS.solveFromFile "/Users/pifi/Documents/solver1/Application/Examples/neg.cl"
     print r
     return ()
-}

ptest = do
      let atms = [0..(2^5)]
          imp = Imply query asrt
          nqarty = 4
          cl = List.foldr f imp [0..nqarty-1]
          f x acc = Forall x acc 
          nq = PredData { pid = 0
                        , parity = nqarty
                        , ptype = DefPred }
          args = List.map Var [0..nqarty-1]
          a = PredData { pid = 1
                       , parity = 1
                       , ptype = DefPred }
          query = NegQuery (pid nq) args
          asrt = Assertion (pid a) [Const 0]
          --result = Solver.solve cl atms
          pmap = IntMap.fromList [(0,"p0"),(1,"p1")]
          --pmap = List.foldl' (\acc pd -> Map.insert (show . pid $ pd) pd acc) Map.empty prds
          amap = List.foldl' (\acc a -> IntMap.insert a ("c" ++ (show a)) acc) IntMap.empty atms
          univcl = List.foldl' (\acc a -> acc ++ "U(c" ++ (show a) ++ ") &") "" atms
          clstr = univcl ++ (showClause cl pmap amap)
      writeFile "/Users/pifi/Documents/solver1/Application/Examples/neg.cl" clstr
      print cl
      --evaluate $ rnf $ result
      return Prelude.True

prop_solve solverData = do
  let depth = maxClauseDepth solverData
      fddDomains = map (\x -> fromIntegral universeSize) 
                   [0..(maxPredicateArity solverData)+depth]
      universeSize = Map.size (Data.atoms solverData)
  fdd_extdomain fddDomains
  let univ = Map.elems (Data.atoms solverData)
      diffResultAsTuples = runDiffSolver solverData
      atms = Map.foldrWithKey 
             (\key val acc -> IntMap.insert val key acc) 
             IntMap.empty 
             (Data.atoms solverData)
      preds = Map.foldrWithKey doit IntMap.empty (predicates solverData)
      doit key val acc = IntMap.insert key' val' acc
           where key' = pid val
                 val' = key
  diffResultAsBdd <- SolversTest.mapResult (fromIntegral depth)
                                           diffResultAsTuples
  bddResult <- runBddSolver solverData
  let g (key, bdd) = do
        let diffBdd =  diffResultAsBdd IntMap.! key
            name = preds IntMap.! key
        {-print name
        print "Diff"
        fdd_printset diffBdd
        print "Bdd"
        fdd_printset bdd -}
        if bdd /= diffBdd
        then do
          print (name ++ " WRONG")
          print "Diff"
          fdd_printset diffBdd
          print "Bdd"
          fdd_printset bdd
        else return ()
  mapM g (IntMap.assocs bddResult)
  return $ bddResult == diffResultAsBdd

bddcheck = verboseCheck (morallyDubiousIOProperty . dummyBdd)
diffcheck = verboseCheck (morallyDubiousIOProperty . prop_diff)
qcheck = quickCheck (morallyDubiousIOProperty . testSolvers)
vcheck = verboseCheck (morallyDubiousIOProperty . testSolvers)

dummyBdd sd = withBdd 100000 10000 $ prop_bdd sd
testSolvers sd = withBdd 100000 10000 $ prop_solve sd
withBdd n m f = bracket_ (bdd_init n m) bdd_done f

prop_diff sd = do
          let universe = Map.elems (Data.atoms sd)
              result = Solver.solve (cls sd) universe
          evaluate $ rnf $ result
          return Prelude.True

prop_bdd sd = do
         let depth = maxClauseDepth sd
             fddDomains = map (\x -> fromIntegral universeSize) 
                              [0..(maxPredicateArity sd)+depth]
             universeSize = Map.size (Data.atoms sd)
         print "before fdd_extdomain"
         fdd_extdomain fddDomains
         print "before fdd_extdomain"
         print (sd :: SolverData Clause)
         bddResult <- runBddSolver sd
         --evaluate $ rnf $ bddResult
         return Prelude.True

-}

replaceArgInClause (Assertion p args) old new = Assertion p args'
  where args' = replaceArgInArgs args old new
replaceArgInClause (And cl1 cl2) old new = And cl1' cl2'
  where cl1' = replaceArgInClause cl1 old new
        cl2' = replaceArgInClause cl2 old new
replaceArgInClause (Imply pre cl) old new = Imply pre' cl'
  where pre' = replaceArgInPre pre old new
        cl' = replaceArgInClause cl old new
replaceArgInClause (Forall x cl) old new = Forall x cl'
  where cl' = replaceArgInClause cl old new
replaceArgInClause TT _ _ = TT

--replaceArgInPre 
replaceArgInPre (AndPre pre1 pre2) old new = AndPre pre1' pre2'
  where pre1' = replaceArgInPre pre1 old new
        pre2' = replaceArgInPre pre2 old new
replaceArgInPre (Or pre1 pre2 n) old new = Or pre1' pre2' n
  where pre1' = replaceArgInPre pre1 old new
        pre2' = replaceArgInPre pre2 old new
replaceArgInPre (Exist x pre n) old new = Exist x pre' n
  where pre' = replaceArgInPre pre old new
replaceArgInPre (ForallPre x pre) old new = ForallPre x pre'
  where pre' = replaceArgInPre pre old new
replaceArgInPre (Query p args) old new = Query p args'
  where args' = replaceArgInArgs args old new
replaceArgInPre (NegQuery p args) old new = NegQuery p args'
  where args' = replaceArgInArgs args old new
replaceArgInPre (Eq a b) old new = Eq a' b'
  where a' = replaceArgInArg old new a
        b' = replaceArgInArg old new b
replaceArgInPre (Neq a b) old new = Neq a' b'
  where a' = replaceArgInArg old new a
        b' = replaceArgInArg old new b
replaceArgInPre pre _ _ = pre

replaceArgInArgs args old new = List.map (replaceArgInArg old new) args

replaceArgInArg old new arg
  | old == arg = new
  | otherwise = arg

tryAsgnRanks sd = foldM f zeroRanks xs
             where xs = List.zip [1..] $ cls sd
                   f acc (i, cl) = asgnRanks cl i acc
                   zeroRanks = Map.fold g IntMap.empty (predicates sd)
                   g pd acc = let id = pid pd
                              in IntMap.insert id 0 acc

asgnRanks (Assertion p _) j preds = asgnR p j preds
asgnRanks (Imply pre cl) j preds = asgnRanks cl j preds
asgnRanks (And cl1 cl2) j preds = asgnRanks cl1 j preds 
                                  >>= asgnRanks cl2 j
asgnRanks (Forall _ cl) j preds = asgnRanks cl j preds

isStratified' :: SolverData [Clause] -> Bool
isStratified' sd = case tryAsgnRanks sd of
                        Just ranks -> isStratified (cls sd) ranks
                        Nothing -> Prelude.False

isStratified cls ranks = List.and . List.map f $ xs
             where xs = List.zip [1..] $ cls
                   f (i, cl) = isStratClause cl i ranks

isStratClause (Assertion p _) i ranks = i == (ranks IntMap.! p)
isStratClause (Imply cond cl) i ranks =
              (isStratCond cond i ranks) && isStratClause cl i ranks
isStratClause (And cl1 cl2) i ranks =
              isStratClause cl1 i ranks && isStratClause cl2 i ranks
isStratClause (Forall _ cl) i ranks = isStratClause cl i ranks

isStratCond (Query p args) i ranks = i >= (ranks IntMap.! p)
isStratCond (NegQuery p args) i ranks = i > (ranks IntMap.! p)
isStratCond (AndPre pre1 pre2) i ranks =
            isStratCond pre1 i ranks && isStratCond pre2 i ranks
isStratCond (Or pre1 pre2 idx) i ranks =
            isStratCond pre1 i ranks && isStratCond pre2 i ranks
isStratCond (ForallPre x pre) i ranks = isStratCond pre i ranks
isStratCond (Exist x pre idx) i ranks = isStratCond pre i ranks
isStratCond _ _ _ = Prelude.True

asgnR pred j preds
      | rank == 0 = Just $ IntMap.insert pred j preds
      | rank == j = Just preds
      | otherwise = Nothing
      where rank = preds IntMap.! pred

toClauseList :: Clause -> [Clause]
toClauseList cl = List.reverse . mkClauseList cl [] $ id

mkClauseList :: Clause -> [Clause] -> ([Clause] -> t) -> t
mkClauseList (Assertion p xs) acc f = f (Assertion p xs:acc)
mkClauseList (Imply pre cl) acc f = f (Imply pre cl:acc)
mkClauseList (And cl1 cl2) acc f = mkClauseList cl1 acc g
                                   where g cls = mkClauseList cl2 (cls ++ acc) f
mkClauseList (Forall x cl) acc f = f (Forall x cl:acc)
mkClauseList TT acc f = f (TT:acc)
