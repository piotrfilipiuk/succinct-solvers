module InflStar where

import qualified Data.IntMap as IntMap(insert,lookup,empty,singleton)
import Debug.Trace(trace)

import DataStar

--register :: Int -> Tuple a -> Consumer a -> Infl a -> Infl a
register pred tuple cons infl = case IntMap.lookup pred infl of
  Just n -> IntMap.insert pred (iterTuple tuple n) infl
  Nothing -> IntMap.insert pred (append tuple) infl
  where --iterTuple ::Tuple a -> InflTrie a -> InflTrie a
        iterTuple t n | trace ("iterTuple: " ++ show t) False = undefined
        iterTuple (BothT [] l) (NodeI cs n) = NodeI (cons:cs) n
	iterTuple (BothT (x:xs) l) (NodeI cs n) = case IntMap.lookup x n of
       	  Just n' -> NodeI cs (IntMap.insert x (iterTuple (BothT xs l) n') n)
          Nothing -> NodeI cs (IntMap.insert x (append (BothT xs l)) n)
        iterTuple (LeftT []) (NodeI cs n) = NodeI (cons:cs) n
        iterTuple (LeftT (x:xs)) (NodeI cs n) = case IntMap.lookup x n of
       	  Just n' -> NodeI cs (IntMap.insert x (iterTuple (LeftT xs) n') n)
          Nothing -> NodeI cs (IntMap.insert x (append $ LeftT xs) n)
        iterTuple (RightT _) (NodeI cs n) = NodeI (cons:cs) n
        iterTuple EmptyT (NodeI cs n) = NodeI (cons:cs) n
        --append :: [Int] -> InflTrie
	append (BothT [] _) = NodeI [cons] IntMap.empty
	append (BothT (x:xs) l) = NodeI [] (IntMap.singleton x (append $ BothT xs l))
        append (LeftT []) = NodeI [cons] IntMap.empty
        append (LeftT (x:xs)) = NodeI [] (IntMap.singleton x (append $ LeftT xs))
        append (RightT _) = NodeI [cons] IntMap.empty
        append EmptyT = NodeI [cons] IntMap.empty

resume :: Int -> Tuple a -> SolverState a -> SolverState a
resume pred tuple ss = case IntMap.lookup pred (getInfl ss) of
  Just n -> invoke $ traverse [] tuple n
    where --traverse :: [(Consumer, [Int])] -> [Int] -> InflTrie -> [(Consumer, [Int])]
          traverse acc (BothT [] l) (NodeI cs _) = foldr (\f acc' -> (f, RightT l):acc') acc cs
          traverse acc (BothT xs@(hd:tl) l) (NodeI cs n) = 
                   case IntMap.lookup hd n of
                        Just n' -> traverse newAcc (BothT tl l) n'
                        Nothing -> newAcc
                   where newAcc = foldr (\f acc' -> (f, (BothT xs l)):acc') acc cs
          traverse acc (LeftT []) (NodeI cs _) = foldr (\f acc' -> (f, EmptyT):acc') acc cs
          traverse acc (LeftT xs@(hd:tl)) (NodeI cs n) =
                   case IntMap.lookup hd n of
                        Just n' -> traverse newAcc (LeftT tl) n'
                        Nothing -> newAcc
                   where newAcc = foldr (\f acc' -> (f, LeftT xs):acc') acc cs
          traverse acc (RightT l) (NodeI cs _) = foldr (\f acc' -> (f, RightT l):acc') acc cs
          invoke = foldr (\(f,ys) acc -> f ys acc) ss
  Nothing -> ss

{-
--register :: Lattice a => Int -> Tuple a -> Consumer a -> Infl a -> Infl a
register pred tuple cons infl = case IntMap.lookup pred infl of
  Just n -> IntMap.insert pred (iterTuple tuple n) infl
  Nothing -> IntMap.insert pred (append tuple) infl
  where --iterTuple :: [Int] -> InflTrie -> InflTrie
        --iterTuple :: Lattice a => Tuple a -> InflTrie a -> InflTrie a
        --iterTuple EmptyT (LeafI
        iterTuple (BothT [] l) (LeafI fs v cs)
                  | v `leq` l = LeafI fs l (cons:cs)
                  | otherwise = LeafI fs v (cons:cs)
        iterTuple (BothT (x:xs) l) (NodeI cs n) = case IntMap.lookup x n of
                  Just n' -> NodeI cs (IntMap.insert x (iterTuple (BothT xs l) n') n)
                  Nothing -> NodeI cs (IntMap.insert x (append (BothT xs l)) n)
        iterTuple (LeftT []) (NodeI cs n) = NodeI (cons:cs) n
        iterTuple (LeftT (x:xs)) (NodeI cs n) = case IntMap.lookup x n of
                  Just n' -> NodeI cs (IntMap.insert x (iterTuple (LeftT xs) n') n)
                  Nothing -> NodeI cs (IntMap.insert x (append (LeftT xs)) n)
        iterTuple (RightT l) (LeafI fs v cs) = undefined
--      iterTuple [] (InflTrie cs mp) = InflTrie (cons:cs) mp
--	iterTuple (hd:tl) (InflTrie cs n) = case IntMap.lookup hd n of
--          Just n' -> InflTrie cs (IntMap.insert hd (iterTuple tl n') n)
--          Nothing -> InflTrie cs (IntMap.insert hd (append tl) n)
        --append :: [Int] -> InflTrie
        --append :: Lattice a => Tuple a -> InflTrie a
        --append :: Tuple a -> InflTrie a
        append (BothT [] l) = LeafI [] l [cons]
        append (BothT (x:xs) l) = NodeI [] $ IntMap.singleton x (append $ BothT xs l)
        append (LeftT []) = NodeI [cons] IntMap.empty
        append (LeftT (x:xs)) = NodeI [] $ IntMap.singleton x (append $ LeftT xs)
        append (RightT l) = LeafI [] l [cons]
--	append [] = InflTrie [cons] IntMap.empty
--	append (hd:tl) = InflTrie [] (IntMap.singleton hd (append tl))
-}
{-
resume :: Int -> [Int] -> Result -> Infl -> MemTab -> (Result, Infl, MemTab)
resume pred tuple result infl memTable = case IntMap.lookup pred infl of
  Just n -> invoke $ traverse [] tuple n
    where traverse :: [(Consumer, [Int])] -> [Int] -> InflTrie -> [(Consumer, [Int])]
          traverse acc [] (InflTrie cs mp) = foldr (\f acc' -> (f, []):acc') acc cs
          traverse acc xs@(hd:tl) (InflTrie cs mp) = 
                   case IntMap.lookup hd mp of
                        Just nextNode -> traverse newAcc tl nextNode
                        Nothing -> newAcc
                        where newAcc = foldr (\f acc' -> (f, xs):acc') acc cs
          invoke = foldr (\(f,ys) acc -> f ys acc) (result, infl, memTable) 
  Nothing -> (result, infl, memTable)
-}
