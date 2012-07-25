module Main where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import Criterion.Main

import Data
import Result
import Solver
import BddSolver
import BddS
import Lexer
import Parser
import SolversTest

import IO
import System.CPUTime
import System.FilePath
import System.Environment
import Control.DeepSeq
import Control.Exception (evaluate)
import Text.PrettyPrint.HughesPJ

countTuples :: Map.Map String [[String]] -> Map.Map String Int
countTuples = Map.map (\xs -> List.length xs)

testBddSolver = do
  contents <- readFile "/home/piotr/HaskellSolver/test.cl"
  BddSolver.solveFromString contents
  putStr "Done.\n"
  
testFiles :: [String]
{-
testFiles = [ "/Users/pifi/Documents/NewRDCls/test100"
            , "/Users/pifi/Documents/NewRDCls/test200"
            , "/Users/pifi/Documents/NewRDCls/test300"
            , "/Users/pifi/Documents/NewRDCls/test400"
            , "/Users/pifi/Documents/NewRDCls/test500"
            , "/Users/pifi/Documents/NewRDCls/test1000" ]
-}

testFiles = [ "/Users/pifi/Downloads/refyiss/test100"
            , "/Users/pifi/Downloads/refyiss/test200"
            , "/Users/pifi/Downloads/refyiss/test300"
            , "/Users/pifi/Downloads/refyiss/test400"
            , "/Users/pifi/Downloads/refyiss/test500" ]

{-
main :: IO ()
main = do
    testData <- mapM createData testFiles
    evaluate $ rnf $ testData
    let benchmarks = map (\(name,cls,univ) -> bench name $ nf (Solver.solve cls) univ) testData
    defaultMain [ bgroup "diffSolver" benchmarks]
-}


{-
main = do
  test100 <- readFile "/Users/pifi/Documents/NewRDCls/test100"
  test200 <- readFile "/Users/pifi/Documents/NewRDCls/test200"
  test300 <- readFile "/Users/pifi/Documents/NewRDCls/test300"
  test400 <- readFile "/Users/pifi/Documents/NewRDCls/test400"
  test500 <- readFile "/Users/pifi/Documents/NewRDCls/test500"
  test1000 <- readFile "/Users/pifi/Documents/NewRDCls/test1000"
  defaultMain [ bgroup "diffSolver" [ bench "100" $ nf Solver.solveFromString test100
                                    , bench "200" $ nf Solver.solveFromString test200
                                    , bench "300" $ nf Solver.solveFromString test300
                                    , bench "400" $ nf Solver.solveFromString test400
                                    , bench "500" $ nf Solver.solveFromString test500
                                    , bench "1000" $ nf Solver.solveFromString test1000 ]
                      ]
-}


main = do
     [inputFile] <- getArgs
     --contents <- readFile "/home/piotr/Downloads/test200"
     contents <- readFile inputFile
     --print $ Solver.solveFromString contents
     --testSolvers contents
     --testSolvers'
{-     bdd0 <- getCPUTime
     BddS.solveFromString contents
     bdd1 <- getCPUTime
     print "bdd time"
     print $ (bdd1 - bdd0) `div` 10^9 -}
     diff0 <- getCPUTime
     --evaluate $ rnf $ (Solver.solveFromString contents)
     let result = Solver.solveFromString contents
     --print $ length $ (result Map.! "rd")
     print result
     diff1 <- getCPUTime
     print "diff time"
     print $ (diff1 - diff0) `div` 10^9
     putStr "Done.\n"

saveAsHornClauses inputFile outputFile = do
  s <- readFile inputFile
  writeFile outputFile (render . hornClausesToDoc . toHornClause (formula . alexScanTokens $ s) Set.empty $ (\ _ x -> x))
{-
main = do
      --[inputFile] <- getArgs
      contents <- readFile "/Users/pifi/Downloads/refyiss/test100"
      --contents <- readFile inputFile
      --(fileName, cls, univ) <- evaluate (createInput contents)
      let (cls, univ) = createInput contents
      --evaluate $ rnf cls
      --print cls
      --evaluate $ rnf $ solve cls $ univ
      measure "solver" [bench "fileName" $ nf (solve cls) $ univ]
      --measure "firstN" [bench "100000" $ nf firstN 100000]
      putStr "Done.\n"

createInput s = (cls solverData, universe)
                    where solverData = createClause . formula . alexScanTokens $ s
                          universe = Map.elems (Data.atoms solverData)
-}
{-
main = do
     [inputFile] <- getArgs
     let (directory, fileName) = splitFileName inputFile
     files <- readFile inputFile
     let inputs = map (createInput . (directory++)) . lines $ files
     let benchmarks = map (\(fileName, cls, univ) -> bench fileName . whnf (solve cls) $ univ) inputs
     measure "solver" benchmarks
-}

--measure name benchmarks = defaultMain [ bgroup name benchmarks ]
