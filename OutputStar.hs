module OutputStar where

import qualified Data.IntMap as IntMap((!),empty,insert)
import qualified Data.Map as Map(mapWithKey,foldrWithKey,toList,fold)

import DataStar
--import Result
import UtilsStar

printSolution
  :: (Lattice a, Show a) => SolverState a -> SolverData a1 -> IO ()
printSolution ss sd = do
              mapM f s
              print "Done."
              print (atoms sd)
              print "Clause"
              print $ cls sd
              where s =  Map.toList . getSolution ss $ sd
                    f (p,t) = do 
                      print p
                      print t
{-
getSolution
  :: (Lattice a, Show a) =>
     SolverState a -> SolverData a1 -> Data.Map.Map String [TupleStr]
-}
getSolution ss sd =
            Map.mapWithKey mapPredicate (predicates sd)
            where swappedAtoms = Map.foldrWithKey swap (IntMap.empty) (atoms sd)
                  swap k a acc = IntMap.insert a k acc
                  mapTuple (BothT xs l) = BothTStr xs' l'
                                          where xs' = mapList xs
                                                l' = show l
                  mapTuple (LeftT xs) = LeftTStr $ mapList xs
                  mapTuple (RightT l) = RightTStr $ show l
                  mapTuple EmptyT = EmptyTStr
                  mapList xs = map (\x -> swappedAtoms IntMap.! x) xs
                  mapPredicate k (idx, _) = map mapTuple . tuples idx $ ss

getSolution' ss sd = 
  Map.fold (\(p,_) acc -> IntMap.insert p (tuples p ss) acc) (IntMap.empty) (predicates sd)