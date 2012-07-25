module DiffUtils
       ( register
       , I.resume
       , tryInsert
       , getTuplesWithPrefix
       , tryMemoize
       , contains
       , R.getTuples
       , getTuples'
       , DiffSSt(..)
       , Result) where

import qualified Data.IntMap as IntMap
import qualified Data.Set as Set

import Infl(DiffSSt(..),Infl,MemoizationTable,Consumer)
import qualified Infl as I(register,resume)
import Result(Result)
import qualified Result as R(getTuplesWithPrefix,tryInsert',contains,getTuples)
import qualified Env as E

register :: Int -> [Int] -> Consumer -> DiffSSt -> DiffSSt
register pred tuple cons ss = ss { infl = inf' }
         where inf = infl ss
               inf' = I.register pred tuple cons inf
{-
resume :: Int -> [Int] -> DiffSSt -> DiffSSt
resume pred tuple ss =
       ss { result = res'
          , infl = inf'
          , memTab = mtab' }
       where res = result ss
             inf = infl ss
             mtab = memTab ss
             (res', inf', mtab') = I.resume pred tuple res inf mtab
-}
tryInsert :: Int -> [Int] -> DiffSSt -> Maybe DiffSSt
tryInsert pred tuple ss = do
          let res = result ss
          res' <- R.tryInsert' pred tuple res
          return ss { result = res' }

contains :: Int -> [Int] -> DiffSSt -> Bool
contains pred tuple ss = R.contains pred tuple res
         where res = result ss

getTuples' :: Int -> DiffSSt -> [[Int]]
getTuples' pred ss = R.getTuples pred res
           where res = result ss

getTuplesWithPrefix :: Int -> [Int] -> DiffSSt -> [[Int]]
getTuplesWithPrefix pred prefix ss = R.getTuplesWithPrefix pred prefix res
                    where res = result ss

tryMemoize :: Int -> E.Env -> DiffSSt -> Maybe DiffSSt
tryMemoize key env ss = do
           let mtab = memTab ss
           mtab' <- case IntMap.lookup key mtab of
                      Just xs -> case Set.member env xs of
                              Prelude.True -> Nothing
                              _ -> Just . IntMap.insert key (Set.insert env xs) $ mtab
                      _ -> Just . IntMap.insert key (Set.singleton env) $ mtab
           return ss { memTab = mtab' }