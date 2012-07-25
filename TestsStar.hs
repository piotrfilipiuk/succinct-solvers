module TestsStar where

import Lexer
import Parser
import LexerStar
import ParserStar
import Data
import DataStar
import SolverStar
import OutputStar
import Solver
import Result

import qualified Data.Map as Map(elems,fold,foldrWithKey)
import qualified Data.Set as Set(fromList)
import qualified Data.IntSet as IntSet(fromList,singleton)
import qualified Data.IntMap as IntMap(IntMap,insert,empty,mapWithKey,(!))

mapArg (Const u) = ConstU u
mapArg (Var x) = VarU x

toClauseStar (Data.Assertion p xs) = DataStar.Assertion p args
                                     where args = LeftA $ map mapArg xs
toClauseStar (Data.And cl1 cl2) = DataStar.And cl1' cl2'
                                  where cl1' = toClauseStar cl1
                                        cl2' = toClauseStar cl2
toClauseStar (Data.Imply pre cl) = DataStar.Imply pre' cl'
                                   where pre' = toPreStar pre
                                         cl' = toClauseStar cl
toClauseStar (Data.Forall x cl) = DataStar.Forall x cl'
                                  where cl' = toClauseStar cl
toClauseStar Data.TT = DataStar.TT


toPreStar (Data.Query p xs) = DataStar.Query p args
                       where args = LeftA $ map mapArg xs
toPreStar (Data.NegQuery p xs) = DataStar.NegQuery p args
                          where args = LeftA $ map mapArg xs
toPreStar (Data.AndPre pre1 pre2) = DataStar.AndPre pre1' pre2'
                                    where pre1' = toPreStar pre1
                                          pre2' = toPreStar pre2
toPreStar (Data.Or pre1 pre2 idx) = error "Need to transform into CNF first..."
toPreStar (Data.Exist x pre idx) = DataStar.Exist x pre' idx
                                   where pre' = toPreStar pre
toPreStar _ = error "Translation not yet implemented."

fromClauseStar (DataStar.Assertion p args) = Data.Assertion p $ fromArgsStar args
fromClauseStar (DataStar.And cl1 cl2) = Data.And cl1' cl2'
               where cl1' = fromClauseStar cl1
                     cl2' = fromClauseStar cl2
fromClauseStar (DataStar.Imply pre cl) = Data.Imply pre' cl'
               where pre' = fromPreStar pre
                     cl' = fromClauseStar cl
fromClauseStar (DataStar.Forall x cl) = Data.Forall x cl'
               where cl' = fromClauseStar cl
fromClauseStar DataStar.TT = Data.TT

fromPreStar (DataStar.Query p args) = Data.Query p $ fromArgsStar args
fromPreStar (DataStar.NegQuery p args) = Data.NegQuery p $ fromArgsStar args
fromPreStar (DataStar.Inc y u) = Data.Eq (Var y) (fromArgUStar u)
fromPreStar (DataStar.AndPre pre1 pre2) = Data.AndPre pre1' pre2'
            where pre1' = fromPreStar pre1
                  pre2' = fromPreStar pre2
fromPreStar (DataStar.Or pre1 pre2 idx) = Data.Or pre1' pre2' idx
            where pre1' = fromPreStar pre1
                  pre2' = fromPreStar pre2
fromPreStar (DataStar.Exist x pre idx) = Data.Exist x pre' idx
            where pre' = fromPreStar pre

fromArgsStar (BothA xs l) = (map fromArgUStar xs) ++ [fromArgLStar l]
fromArgsStar (LeftA xs) = map fromArgUStar xs
fromArgsStar (RightA l) = [fromArgLStar l]
fromArgsStar EmptyA = []
fromArgUStar (VarU x) = Var x
fromArgUStar (ConstU a) = Const a
fromArgLStar (VarL y) = Var y
fromArgLStar (AbsL u) = fromArgUStar u

fromTupleStar (BothT xs l) = map (\v -> xs ++ [v]) vs
              where vs = betaInv l
fromTupleStar (LeftT xs) = [xs]
fromTupleStar (RightT l) = map (\v -> [v]) $ betaInv l

runDiffSolver solverData = 
  Map.fold (\(n,t) acc -> IntMap.insert n (getTuples n result) acc) IntMap.empty preds
  where universe = Map.elems (Data.atoms solverData)
        preds = (Data.predicates solverData)
        result = Solver.solve (Data.cls solverData) universe

toSolverDataStar ds =
                 DataStar.SolverData
                        { DataStar.cls = toClauseStar . Data.cls $ ds
                        , DataStar.predicates = Data.predicates ds
                        , DataStar.atoms = Data.atoms ds
                        , DataStar.maxPredicateArity = Data.maxPredicateArity ds
                        , DataStar.maxClauseDepth = Data.maxClauseDepth ds }

fromSolverDataStar ds =
                 Data.SolverData
                        { Data.cls = fromClauseStar . DataStar.cls $ ds
                        , Data.predicates = DataStar.predicates ds
                        , Data.atoms = DataStar.atoms ds
                        , Data.maxPredicateArity = DataStar.maxPredicateArity ds
                        , Data.maxClauseDepth = DataStar.maxClauseDepth ds }

runSolverStar solverData = getSolution' ss solverData
  where universe = Map.elems (DataStar.atoms solverData)
        all = IntSet.fromList $ universe
        idToNames = Map.foldrWithKey (\k v acc -> IntMap.insert v k acc) IntMap.empty (DataStar.atoms solverData)
        topL = top all
        complL = compl all
        beta' x = IntSet.singleton x
--          where str = idToNames IntMap.! x
        ss = SolverStar.solve (DataStar.cls solverData) universe topL complL beta'

runAndCompare sd = IntMap.mapWithKey comparePred diff
              where diff = runDiffSolver sd
                    star = runSolverStar . toSolverDataStar $ sd
                    comparePred p xs = let ys = star IntMap.! p
                                       in if compareTuples xs ys
                                          then Prelude.True
                                          else error $ "Different solutions for predicate" ++ (show p)
                    compareTuples xs ys = xs' == ys'
                                  where ys' = Set.fromList . map toListLeftT $ ys
                                        xs' = Set.fromList xs
                    toListLeftT (LeftT xs) = xs

runAndCompareStar sd = IntMap.mapWithKey comparePred diff
              where diff = runDiffSolver . fromSolverDataStar $ sd
                    star = runSolverStar sd
                    comparePred p xs = let ys = star IntMap.! p
                                       in if compareTuples xs ys
                                          then Prelude.True
                                          else error $ "Different solutions for predicate" ++ (show p) ++ (show xs) ++ (show $ concat . map fromTupleStar $ ys)
                    compareTuples xs ys = xs' == ys'
                                  where ys' = Set.fromList . concat . map fromTupleStar $ ys
                                        xs' = Set.fromList xs

runComplexTests files g =
  mapM (f) files
  where f file = do
          contents <- readFile file
          print file
          return $ g contents

runAll = do
       let _ = runSimpleTests
       runComplexTests files (runAndCompare . createCl)
       runComplexTests filesStar (runAndCompareStar . createClStar)

runSimpleTests = map (runAndCompareStar . createClStar) clausesStar

createCl = Data.createClause . Parser.formula . Lexer.alexScanTokens
createClStar = DataStar.createClause . ParserStar.formula . LexerStar.alexScanTokens

clausesStar = [ "R(a;[b]) & R(a;[c]) & (A x. A y. R(x;y) => S(x;y))"
              , "R(a;[b]) & R(b;[c]) & Q(a;) & (A x. A y. (Q(x;) & R(x;y) => S(x;y)))"
              , "R(a;[b]) & R(b;[c]) & Q(a;) & (A x. A z. A y. (R(x;y) & y(z) => S(x,z;)))"
              , "R(a;[b]) & R(b;[c]) & (A x. A y. (R(x;y) => S(;[x])))"
              , "R(a;[b]) & R(b;[c]) & Q(;[b]) & (A x. A y. ((R(x;y) & Q(;y)) => S(;[x])))"
              , "R(a;[b]) & R(b;[c]) & Q(;[b]) & (A x. ((E y. (R(x;y) & Q(;y))) => S(;[x])))"
              , "R(a,t;[b]) & R(b,t;[c]) & Q(;[b]) & (A x. ((E z. E y. (R(x,z;y) & Q(;y))) => S(;[x])))"
              , "R(a;[b]) & R(b;[c]) & (A x. A y. ((R(x;y) & Q(;y)) => S(;[x])))"
              , "R_b(;[s3]) & T(s1,t;[s2]) & T(s2,t;[s2]) & T(s2,t;[s3]) & T(s3,t;[s2]) & T(s4,t;[s3]) & T(s4,t;[s5]) & (A s. A s'. ((R_b(;s') & T(s,t;s')) => F(;[s])))"
              ]

files = [ "/Users/pifi/Documents/TestData/rd01.cl"
        , "/Users/pifi/Documents/TestData/existsPre01.cl"
        , "/Users/pifi/Documents/TestData/existsPre02.cl"
        , "/Users/pifi/Documents/TestData/ACTL_EX01.cl"
        , "/Users/pifi/Documents/TestData/ACTL_EX02.cl"
        , "/Users/pifi/Documents/TestData/ACTL_EU01.cl"
        , "/Users/pifi/Documents/TestData/ACTL_EU02.cl"
        ]

filesStar = [ "/Users/pifi/Documents/rd01.clstar"
            , "/Users/pifi/Documents/EX01.clstar"
            , "/Users/pifi/Documents/EU01.clstar"
            , "/Users/pifi/Documents/EU01a.clstar"
            ]