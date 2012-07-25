--module Main where
module SolversTest where

import Foreign.C.Types
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
--import qualified Data.List as List
import Control.Monad

import Lexer
import Parser
import Data
import Result
import Solver
import BddSolver
import Bdd
import Fdd
import BddUtils
--import LayerSolver(solve)
import List(foldl,map)
import ALFPLogic
import Magic
  
--main = runAllTests

testMagic filePath = do
  content <- readFile filePath
  let solverData = toSolverData . formula . alexScanTokens $ content
      p =  pid $ (predicates solverData) Map.! "R"
      args = [Const 0]
      clauseList = toClauseList . cls $ solverData
  print "Starting magic."
  let sd = doMagic (solverData { cls = clauseList }) (p, args)
      pmap = List.foldl (\acc (str,pd) -> IntMap.insert (pid pd) str acc)
             IntMap.empty . Map.assocs $ (predicates sd)
      amap = List.foldl (\acc (str,n) -> IntMap.insert n str acc)
             IntMap.empty . Map.assocs $ (atoms sd)
      xs = List.map (\cl -> showClause cl pmap amap) . cls $ sd
  print xs

cl1 = "R(b,a) & T(a,b) & (A x. A y. A z. T(x,y) & R(y,z) => R(x,z))"

testMagicStr content = do
  let solverData = toSolverData . formula . alexScanTokens $ content
      p =  pid $ (predicates solverData) Map.! "R"
      args = [Const 0, Var 1]
      clauseList = toClauseList . cls $ solverData
  print "Starting magic."
  let sd = doMagic (solverData { cls = clauseList }) (p, args)
      pmap = List.foldl (\acc (str,pd) -> IntMap.insert (pid pd) str acc)
             IntMap.empty . Map.assocs $ (predicates sd)
      amap = List.foldl (\acc (str,n) -> IntMap.insert n str acc)
             IntMap.empty . Map.assocs $ (atoms sd)
      xs = List.map (\cl -> showClause cl pmap amap) . cls $ sd
  print xs

--files = ["/Users/pifi/Documents/foo.cl"]
--files = ["/Users/pifi/Documents/blah.cl"]
--files = ["/Users/pifi/Documents/solver1/Application/Examples/neg.cl"]
--files = ["/Users/pifi/Documents/TestData/simple.cl"]
rfiles = ["/Users/pifi/Documents/TestData/neg.cl"]

files = ["/Users/pifi/Documents/TestData/rd100.cl",
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

runAllTests = mapM (f) files
               where f file = do
                              contents <- readFile file
                              print file
                              compareResults . toSolverData . formula . alexScanTokens $ contents

testSolvers = compareResults . toSolverData . formula . alexScanTokens

toStr res prds atms = result
      where result = IntMap.foldWithKey f Map.empty res
            f key val acc = Map.insert p xs acc
              where p = prds IntMap.! key
                    xs = map (map (atms IntMap.!)) val

runDiffSolver :: SolverData Clause -> IntMap.IntMap [[Int]]
runDiffSolver solverData = Map.fold f IntMap.empty (Data.predicates solverData)
  where universe = Map.elems (Data.atoms solverData)
        result = Solver.solve solverData
        f pd acc = IntMap.insert (pid pd) (ys (pid pd)) acc
        ys p = case IntMap.lookup p result of
                 Just t -> tuples t
                 _ -> []

runBddSolver :: SolverData Clause -> IO (IntMap.IntMap CInt)
runBddSolver solverData = BddSolver.solve solverData

compareResults solverData = do
  let fddDomains = map (\x -> fromIntegral universeSize) [0..(maxPredicateArity solverData)+(maxClauseDepth solverData)]
      universeSize = Map.size (Data.atoms solverData)
  bdd_init 100000 10000
  fdd_extdomain fddDomains
  bddResult <- runBddSolver solverData
  diffResult <- mapResult (fromIntegral . maxClauseDepth $ solverData) $ runDiffSolver solverData
--  diffResult <- mapResult (fromIntegral . maxClauseDepth $ solverData) $ (modify $ runDiffSolver solverData)
  --print $ runDiffSolver solverData
  let g (key, bdd) = do
                  let bdd' = bddResult IntMap.! key
                  if bdd == bdd' then print $ (show key)++" OK" else print $ (show key)++" WRONG"
                  fdd_printset bdd
                  fdd_printset bdd'
                  --print $ (show key)++" "++(show bdd)++" "++(show bdd')
  mapM g (IntMap.assocs diffResult)
  --print $ "diff# "++(show $ IntMap.size diffResult)
  --print $ "bdd# "++(show $ IntMap.size bddResult)
  if bddResult == diffResult then print "Results are the same."  else print "Results differ."
  bdd_done
  --return x
{-  
tuplesToBdd :: Integral b => CInt -> [[b]] -> IO CInt
tuplesToBdd fstIdx xs = foldM addTuple 0 xs
  where addTuple bdd xs = do
          (_,bdd') <- foldM updateVar (fstIdx,1) xs
          bdd_or bdd bdd'
        updateVar (i,bdd) x = do
          bdd' <- fdd_ithvar i (fromIntegral x) >>= bdd_and bdd
          return (i+1,bdd')
-}
mapResult i result = (return . IntMap.fromList) =<< (mapM f . IntMap.assocs . IntMap.map (tuplesToBdd i) $ result)
               where f (key,elem) = do
                       elem' <- elem
                       return (key, elem')