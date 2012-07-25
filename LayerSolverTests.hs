--module LayerSolverTests where
module Main where

--import Solver(getResult')
import Data(SolverData(..),PredData(..),Argument(..),showClause)
import SolversTest(runBddSolver,runDiffSolver,mapResult,toStr)
import LFPSolver(solve,createSolverData)
import LFPLogic(toALFPSolverData,LFPLayer,isStratified')
import Bdd
import BddIO(readFddFile)
import Fdd
import Foreign.C.Types
import qualified Data.IntMap as IntMap(assocs,(!),map,intersection,empty,insert,size,toList,fromList)
import qualified Data.Map as Map(size,foldrWithKey,fold,elems)
import qualified Data.Set as Set(fromList)
import BddUtils

import Control.DeepSeq(rnf)
import Control.Exception(bracket_,evaluate)
--import Debug.Trace(trace)

import ArbLFP

import Test.QuickCheck
import Test.QuickCheck.Property (Result(..),morallyDubiousIOProperty)
--import Test.QuickCheck.Monadic(monadicIO)

import qualified Solver as Solver

--main = stratificationCheck
--main = qcheck
main = diffcheck
--main = qcheckLFP

qcheck = quickCheck (morallyDubiousIOProperty . testSolvers)
vcheck = verboseCheck (morallyDubiousIOProperty . testSolvers)
diffcheck = verboseCheck (morallyDubiousIOProperty . prop_diff)
--qcheck = quickCheck (monadicIO . prop_solve)
qcheckLFP = quickCheck (morallyDubiousIOProperty . testLFP)
stratificationCheck = quickCheck prop_stratified

{-
runTests s = testSolvers sd
         where sd = createSolverData s
-}
--withBdd n m f = bracket_ (bdd_init n m) bdd_done f

testSolvers sd = withBdd 100000 10000 $ prop_solve sd
testLFP sd = withBdd 100000 10000 $ prop_LFP sd

prop_stratified :: SolverData [LFPLayer] -> Bool
prop_stratified = isStratified'

prop_diff sd = do
          let sd' = toALFPSolverData sd
              universe = Map.elems (Data.atoms sd')
              result = Solver.solve sd'
          evaluate $ rnf $ result
          return True

prop_LFP sd = do
         fdd_extdomain . fddDomains $ sd
         res <- runLayerSolver sd
         print res
         --evaluate $ rnf $ res
         return Prelude.True          

--dummy_check = verboseCheck prop_dummy

--It should not be called directly, since it does not call bdd_init and bdd_done.
prop_solve solverData = do
  --print "===================TEST====================="
  let alfpsd = toALFPSolverData solverData
      maxDepth = max (maxClauseDepth solverData) (maxClauseDepth alfpsd)
      fddDomains = map (\x -> fromIntegral universeSize) 
                   [0..(maxPredicateArity solverData)+maxDepth]
      universeSize = Map.size (Data.atoms solverData)
  fdd_extdomain fddDomains
  let --diffResultAsTuples = runDiffSolver alfpsd
      --diffResultAsSets = IntMap.map Set.fromList diffResultAsTuples
      univ = Map.elems (Data.atoms solverData)
      arities = Map.fold getArty IntMap.empty (predicates alfpsd)
      getArty pd acc = IntMap.insert (pid pd) (parity pd) acc
      atms = Map.foldrWithKey 
             (\key val acc -> IntMap.insert val key acc) 
             IntMap.empty 
             (Data.atoms alfpsd)
      preds = Map.foldrWithKey doit IntMap.empty (predicates alfpsd)
      doit key val acc = IntMap.insert key' val' acc
           where key' = pid val
                 val' = key
  --print "---ALFP solver data--------------"
  --print alfpsd
  --print solverData
  --let bbb = Solver.solve (cls alfpsd) univ
  --print bbb
  --print "---------------------------------"
  --print diffResultAsTuples
  --diffResultAsBdd <- SolversTest.mapResult (fromIntegral maxDepth)
  --                                         diffResultAsTuples
  bddResult <- runBddSolver alfpsd
  layerResult <- runLayerSolver solverData { maxClauseDepth = maxDepth }
  --let layerResult = IntMap.empty
  --print layerResult
  --print diffResultAsTuples
  --print bddResult
  layerResultAsLists <- bddsToTuples univ arities maxDepth layerResult
  bddResultAsLists <- bddsToTuples univ arities maxDepth bddResult
  let bddResult' = IntMap.intersection bddResult layerResult
      --diffResultAsBdd' = IntMap.intersection diffResultAsBdd layerResult
      --diffResultAsSets' = IntMap.intersection diffResultAsSets layerResult
      layerResultAsSets = IntMap.map Set.fromList layerResultAsLists
      bddResultAsSets = IntMap.map Set.fromList bddResultAsLists
      bddResultAsSets' = IntMap.intersection bddResultAsSets layerResultAsSets
      --ok = (diffResultAsBdd' == layerResult) && (layerResult == bddResult')
      ok = (layerResult == bddResult')
      g (key, bdd) = do
        let --diffBdd =  diffResultAsBdd' IntMap.! key
            bdd' = bddResult' IntMap.! key
            name = preds IntMap.! key
        if bdd == bdd'
        then return ()
        else do
             print $ name ++ " WRONG"
             print "LFP"
             fdd_printset bdd
             print "Bdd"
             fdd_printset bdd'
      f (key, layeredSet) = do
        let --diffSet =  diffResultAsSets IntMap.! key
            bddSet = bddResultAsSets' IntMap.! key
            name = preds IntMap.! key
        print name
        if layeredSet == bddSet then print "OK" else print "WRONG"
        print "LFP"
        print layeredSet
        print "Bdd"
        print bddSet
      --okTuples = layerResultAsSets == diffResultAsSets'
  --print "LFP"
  --print layerResultAsSets
  --print "Diff"
  --print diffResultAsSets'
  --writeFile "/Users/pifi/Documents/dump" $ showClause (cls alfpsd) preds atms
  --mapM g (IntMap.assocs layerResult)
  --mapM f (IntMap.assocs layerResultAsSets)
  --return $ ok && okTuples
  return ok
  --return True
  
--files = ["/Users/pifi/Documents/TestData/ae4"]
--files = ["/Users/pifi/Documents/ctl"]
files = ["/Users/pifi/Documents/foo.cl"]

runAll = runTests files

runTests :: [FilePath] -> IO ()
runTests files = do
         bs <- mapM f files
         print bs
         if and bs 
         then print "OK, all tests passed."
         else print "Failure."
         where f file = do
                 contents <- readFile file
                 testSolvers . createSolverData $ contents

runLayerSolver solverData = LFPSolver.solve solverData

{-
bddsToTuples univ arities startIdx result = do
             tuples <- mapM f . IntMap.toList $ result
             return . IntMap.fromList $ tuples
             where f (key, bdd) = do
                     let filePath = "./temp" ++ (show key) ++ ".fdd"
                         arity = arities IntMap.! key
                     fdd_fprintset filePath bdd
                     xs <- readFddFile univ arity startIdx filePath
                     return (key, xs)
-}
saveBdds bdds = mapM (\(a,b) -> fdd_fprintset (filePath a) b) . IntMap.toList $ bdds
         where filePath id = "./temp" ++ (show id) ++ ".fdd"



{-
run solverData = do
  let fddDomains = map (\x -> fromIntegral universeSize) [0..(maxPredicateArity solverData)+(maxClauseDepth solverData)]
      universeSize = Map.size (Data.atoms solverData)
  bdd_init 100000 10000
  fdd_extdomain fddDomains
  let alfpsd = toALFPSolverData solverData
  diffResult <- mapResult (fromIntegral . maxClauseDepth $ solverData) $ runDiffSolver alfpsd
  bddResult <- runBddSolver alfpsd
  layerResult <- runLayerSolver solverData
  let bddResultCutted = IntMap.intersection bddResult layerResult
      diffResultCutted = IntMap.intersection diffResult layerResult
  let preds = Map.foldrWithKey doit IntMap.empty (predicates alfpsd)
      doit key val acc = IntMap.insert key' val' acc
                         where key' = pid val
                               val' = key
  let g (key, bdd) = do
                  let diffBdd = diffResult IntMap.! key
                      bdd' = bddResult IntMap.! key
                      name = preds IntMap.! key
                  if bdd == bdd' then print $ name ++" OK" else print $ name ++" WRONG"
                  print name
                  let filePath = "/Users/pifi/Documents/"++name++".fdd"
                  print $ "Saving fdd to file: " ++ filePath
                  fdd_fprintset filePath bdd
                  print "layered:\n"
                  fdd_printset bdd
                  print "bdd:\n"
                  fdd_printset bdd'
                  print "diff:\n"
                  fdd_printset diffBdd
      h (key, bdd) = do
                  let --diffBdd = diffResult IntMap.! key
                      bdd' = bddResult IntMap.! key
                      name = preds IntMap.! key
                  if bdd == bdd' then print $ name ++" OK" else print $ name ++" WRONG"
                  print name
                  let filePath = "/Users/pifi/Documents/" ++ name ++ ".fdd"
                  print $ "Saving fdd to file: " ++ filePath
                  fdd_fprintset filePath bdd
                  --print "layered:\n"
                  --fdd_printset bdd
                  print "bdd:\n"
                  fdd_printset bdd'
                  print "diff:\n"
                  fdd_printset bdd
  mapM g (IntMap.assocs layerResult)
  --print $ if IntMap.size bddResult == IntMap.size diffResult then "Sizes equal." else "Sizes do not equal."
  --mapM h (IntMap.assocs diffResult)
  print diffResult
  print bddResult
  let atms = Map.foldrWithKey 
             (\key val acc -> IntMap.insert val key acc) 
             IntMap.empty 
             (Data.atoms alfpsd)
      dr = toStr (runDiffSolver $ alfpsd) preds atms
  --print . showClause (cls alfpsd) preds $ atms
  print dr
  writeFile "/Users/pifi/Documents/dump" $ showClause (cls alfpsd) preds atms
  print $ cls solverData
  print $ predicates alfpsd
  print $ atoms alfpsd
  print "Universe size ="
  print $ Map.size . atoms $ solverData
  --print $ "diff# "++(show $ IntMap.size diffResult)
  --print $ "bdd# "++(show $ IntMap.size bddResult)
  if bddResultCutted == layerResult then print "Results are the same."  else print "Results differ."
  if diffResult == bddResult then print "Bdd_Diff results are the same."  else print "Bdd_Diff results differ."
  bdd_done
  --return x

-}