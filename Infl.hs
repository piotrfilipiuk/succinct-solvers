module Infl where

import Result
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set

type MemoizationTable = IntMap.IntMap (Set.Set (IntMap.IntMap (Maybe Int)))
type Consumer = [Int] -> DiffSSt -> DiffSSt

data InflTrie = InflTrie [Consumer] (IntMap.IntMap InflTrie)

type Infl = IntMap.IntMap InflTrie

data DiffSSt = DiffSSt { result :: Result
                       , infl :: Infl
                       , memTab :: MemoizationTable }

register :: Int -> [Int] -> Consumer -> Infl -> Infl
register pred tuple cons infl = case IntMap.lookup pred infl of
  Just n -> IntMap.insert pred (iterTuple tuple n) infl
  Nothing -> IntMap.insert pred (append tuple) infl
  where iterTuple :: [Int] -> InflTrie -> InflTrie
        iterTuple [] (InflTrie cs mp) = InflTrie (cons:cs) mp
	iterTuple (hd:tl) (InflTrie cs n) = case IntMap.lookup hd n of
       	  Just n' -> InflTrie cs (IntMap.insert hd (iterTuple tl n') n)
          Nothing -> InflTrie cs (IntMap.insert hd (append tl) n)
        append :: [Int] -> InflTrie
	append [] = InflTrie [cons] IntMap.empty
	append (hd:tl) = InflTrie [] (IntMap.singleton hd (append tl))

resume :: Int -> [Int] -> DiffSSt -> DiffSSt
resume pred tuple ss = case IntMap.lookup pred inf of
  Just n -> invoke $ traverse [] tuple n
    where traverse :: [(Consumer, [Int])] -> [Int] -> InflTrie -> [(Consumer, [Int])]
          traverse acc [] (InflTrie cs mp) = foldr (\f acc' -> (f, []):acc') acc cs
          traverse acc xs@(hd:tl) (InflTrie cs mp) = 
                   case IntMap.lookup hd mp of
                        Just nextNode -> traverse newAcc tl nextNode
                        Nothing -> newAcc
                   where newAcc = foldr (\f acc' -> (f, xs):acc') acc cs
          invoke = foldr (\(f,ys) acc -> f ys acc) ss
  Nothing -> ss
  where inf = infl ss