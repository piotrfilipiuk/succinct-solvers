--module Test where
module Main where

import Test.QuickCheck
import Test.QuickCheck.Property (Result(..),morallyDubiousIOProperty)

import qualified Data.IntMap as IntMap(insert,empty,intersection,assocs,(!),fromList,map)
import qualified Data.Map as Map(size,elems,fold,foldrWithKey)
import Control.DeepSeq(rnf)
import Control.Exception(evaluate)

import qualified Solver as Solver
import qualified BddSolver as BddSolver
import qualified LFPSolver as LFPSolver
import LFPLogic
--import ALFPLogic
import Data
import BddUtils(fddDomains,BddSSt(..),withBdd,tuplesToBdd)
import Fdd(fdd_extdomain,fdd_printset)
import ArbLFP

--main = qcheckLFP >> qcheckALFP
main = quickCheck $ morallyDubiousIOProperty . prop_diff

qcheckLFP = quickCheck (morallyDubiousIOProperty . (run bddVsLFP))
qcheckALFP = quickCheck (morallyDubiousIOProperty . (run diffVsBdd))

run f sd = withBdd 100000 10000 $ f sd

--It should not be called directly, since it does not call bdd_init and bdd_done.
bddVsLFP sd = do
  let alfpsd = toALFPSolverData sd
      maxDepth = max (maxClauseDepth sd) (maxClauseDepth alfpsd)
      solverData = sd { maxClauseDepth = maxDepth }
      fddDoms = fddDomains $ solverData
      universeSize = Map.size . atoms $ solverData
  fdd_extdomain fddDoms
  let univ = Map.elems (atoms solverData)
      arities = Map.fold getArty IntMap.empty (predicates alfpsd)
      getArty pd acc = IntMap.insert (pid pd) (parity pd) acc
      atms = Map.foldrWithKey 
             (\key val acc -> IntMap.insert val key acc) 
             IntMap.empty 
             (atoms alfpsd)
      preds = Map.foldrWithKey doit IntMap.empty (predicates alfpsd)
      doit key val acc = IntMap.insert key' val' acc
           where key' = pid val
                 val' = key
  bddResult <- BddSolver.solve alfpsd
  layerResult <- LFPSolver.solve solverData
  --layerResultAsLists <- bddsToTuples univ arities maxDepth layerResult
  --bddResultAsLists <- bddsToTuples univ arities maxDepth bddResult
  let bddResult' = IntMap.intersection bddResult layerResult
      --layerResultAsSets = IntMap.map Set.fromList layerResultAsLists
      --bddResultAsSets = IntMap.map Set.fromList bddResultAsLists
      --bddResultAsSets' = IntMap.intersection bddResultAsSets layerResultAsSets
      ok = (layerResult == bddResult')
  if not ok then print alfpsd else return ()
  verboseCompareBdds layerResult bddResult' preds
  return ok

diffVsBdd solverData = do
  let depth = maxClauseDepth solverData
      universeSize = Map.size (Data.atoms solverData)
  fdd_extdomain . fddDomains $ solverData
  let univ = Map.elems (Data.atoms solverData)
      diffResultAsTuples = Solver.trieToTuples (Solver.solve $ solverData) solverData
  diffResultAsBdd <- mapResult (fromIntegral depth)
                               diffResultAsTuples
  bddResult <- BddSolver.solve solverData
  return $ bddResult == diffResultAsBdd

mapResult i result = 
  (return . IntMap.fromList) 
  =<< (mapM f . IntMap.assocs . IntMap.map (tuplesToBdd i) $ result)
  where f (key,elem) = do
          elem' <- elem
          return (key, elem')

verboseCompareBdds res1 res2 preds = mapM g (IntMap.assocs res2)
               where g (key, bdd) = do
                     let bdd' = res1 IntMap.! key
                         name = preds IntMap.! key
                     if bdd == bdd'
                     then return ()
                     else do
                          print $ name ++ " WRONG"
                          print "res1"
                          fdd_printset bdd
                          print "res2"
                          fdd_printset bdd'

verboseCompareSets res1 res2 preds = mapM g (IntMap.assocs res2)
               where g (key, s) = do
                     let s' = res1 IntMap.! key
                         name = preds IntMap.! key
                     if s == s'
                     then return ()
                     else do
                          print $ name ++ " WRONG"
                          print "res1"
                          print s
                          print "res2"
                          print s'

prop_diff sd = do
          let result = Solver.solve sd
          evaluate $ rnf $ result
          return Prelude.True

filesALFP = ["/Users/pifi/Documents/TestData/rd100.cl",
         "/Users/pifi/Documents/TestData/rd200.cl",
         "/Users/pifi/Documents/TestData/rd01.cl",
         "/Users/pifi/Documents/TestData/rd02.cl",
         "/Users/pifi/Documents/TestData/existsPre01.cl",
         "/Users/pifi/Documents/TestData/existsPre02.cl",
         "/Users/pifi/Documents/TestData/forallPre01.cl",
         "/Users/pifi/Documents/TestData/forallPre02.cl",
         "/Users/pifi/Documents/TestData/mcAX01.cl",
         "/Users/pifi/Documents/TestData/ACTL_EX01.cl",
         "/Users/pifi/Documents/TestData/ACTL_EX02.cl",
         "/Users/pifi/Documents/TestData/ACTL_EU01.cl",
         "/Users/pifi/Documents/TestData/ACTL_EU02.cl",
         "/Users/pifi/Documents/TestData/ACTL_AX01.cl",
         "/Users/pifi/Documents/TestData/ACTL_AX02.cl",
         "/Users/pifi/Documents/TestData/ACTL_AX03.cl",
         "/Users/pifi/Documents/TestData/ACTL_AX04.cl",
         "/Users/pifi/Documents/TestData/ACTL_AX05.cl",
         "/Users/pifi/Documents/TestData/ACTL_AX06.cl",
         "/Users/pifi/Documents/TestData/ACTL_AU01.cl",
         "/Users/pifi/Documents/TestData/ACTL_AU02.cl",
         "/Users/pifi/Documents/TestData/ACTL_AU03.cl",
         "/Users/pifi/Documents/TestData/ACTL_AU04.cl",
         "/Users/pifi/Documents/TestData/simple.cl",
         "/Users/pifi/Documents/TestData/neg.cl"
        ]

filesLFP = ["/Users/pifi/Documents/foo.cl"]

runAllALFPTests = mkSolverData BddSolver.createSolverData filesALFP >>= mapM (run diffVsBdd)

runAllLFPTests = mkSolverData LFPSolver.createSolverData filesLFP >>= mapM (run bddVsLFP)

mkSolverData f = mapM g
             where g path = do
                     contents <- readFile path
                     return . f $ contents

{-
runLFPTests :: [FilePath] -> IO ()
runLFPTests files = do
            bs <- mapM f files
            if and bs 
            then print "OK, all tests passed."
            else print "Failure."
            where f file = do
                  contents <- readFile file
                  run bddVsLFP $ LayerSolver.createSolverData contents
-}