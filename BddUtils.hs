module BddUtils where

import qualified Data.IntMap as IntMap(IntMap,(!),fromList,toList,insert,empty,foldWithKey)
import qualified Data.Map as Map(Map,mapWithKey,foldrWithKey,size,empty,insert)
import qualified Data.Set as Set(Set,fromList)
import Control.Monad
import System.Directory
import System.IO
import Control.Exception

import Data(PredData(..),SolverData(..))
import Result(getTuples)

import Bdd(bdd_init,bdd_done,bdd_or,bdd_and)
import Fdd(fdd_fprintset,fdd_ithvar)
import BddIO(readFddFile)
import Foreign.C.Types

bddsToTuples
  :: [Int]
     -> IntMap.IntMap Int
     -> Int
     -> IntMap.IntMap Foreign.C.Types.CInt
     -> IO (IntMap.IntMap [[Int]])
bddsToTuples univ arities startIdx result = do
             tuples <- mapM f . IntMap.toList $ result
             return . IntMap.fromList $ tuples
             where f (key, bdd) = do
                     let filePath = "./temp" ++ (show key) ++ ".fdd"
                         arity = arities IntMap.! key
                         run = do
                             fdd_fprintset filePath bdd
                             xs <- readFddFile univ arity startIdx filePath
                             return (key, xs)
                         clean = removeFile filePath
                     finally run clean

tuplesToBdd :: Integral b => CInt -> [[b]] -> IO CInt
tuplesToBdd fstIdx xs = foldM addTuple 0 xs
  where addTuple bdd xs = do
          (_,bdd') <- foldM updateVar (fstIdx,1) xs
          bdd_or bdd bdd'
        updateVar (i,bdd) x = do
          bdd' <- fdd_ithvar i (fromIntegral x) >>= bdd_and bdd
          return (i+1,bdd')

withBdd n m f = bracket_ (bdd_init n m) bdd_done f

{-
fromBddResult :: SolverData a -> Result.Result -> Map.Map String [[String]]
fromBddResult sd bddres = 
              Map.mapWithKey mapPredicate (predicates sd)
              where swappedAtoms = Map.foldrWithKey 
                                                    (\k a acc -> IntMap.insert a k acc) 
                                                    (IntMap.empty) 
                                                    (atoms sd)
                    mapTuple xs = map (\x -> swappedAtoms IntMap.! x) xs
                    mapPredicate k pd = map mapTuple . getTuples (pid pd) $ bddres
-}

fromIntMapResult :: SolverData a -> IntMap.IntMap [[Int]] -> Map.Map String [[String]]
fromIntMapResult sd res = IntMap.foldWithKey f Map.empty res
          where f key xs acc = let p = prds IntMap.! key
                                   ys = map (map (atms IntMap.!)) xs
                               in Map.insert p ys acc
                atms = swapKeysWithElems . atoms $ sd
                prds = swapPredIdsWithNames . predicates $ sd

--fddDomains :: SolverData a -> [Int]
fddDomains sd = map (\x -> fromIntegral univSize) [0..end]
           where univSize = Map.size . atoms $ sd
                 end = (maxPredicateArity sd) + (maxClauseDepth sd)

swapKeysWithElems :: Map.Map k Int -> IntMap.IntMap k
swapKeysWithElems = Map.foldrWithKey f IntMap.empty
                  where f key elem acc = IntMap.insert elem key acc

swapPredIdsWithNames :: Map.Map k PredData -> IntMap.IntMap k
swapPredIdsWithNames = Map.foldrWithKey f IntMap.empty
                     where f key elem acc = IntMap.insert (pid elem) key acc

data BddSSt = BddSSt { result :: Result
                     , infl :: Infl
                     , memTab :: MemTable }

type Result = IntMap.IntMap CInt
type MemTable = IntMap.IntMap (Set.Set CInt)
newtype InflSet = InflSet [CInt -> BddSSt -> IO BddSSt]
type Infl = IntMap.IntMap InflSet

register pred f ss = ss { infl = inf' }
         where inf = infl ss
               InflSet inflSet = inf IntMap.! pred
               inf' = IntMap.insert pred (InflSet $ f:inflSet) . infl $ ss

resume pred bdd ss = foldM (\acc f -> f bdd acc) ss inflSet
       where inf = infl ss
             (InflSet inflSet) = inf IntMap.! pred

getContent pred ss = res IntMap.! pred
           where res = result ss

insert pred bdd ss = ss { result = res }
       where res = IntMap.insert pred bdd . result $ ss