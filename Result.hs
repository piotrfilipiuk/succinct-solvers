module Result where

import qualified Data.IntMap as IntMap
import Control.DeepSeq

data ResultTrie = ResultTrie (IntMap.IntMap ResultTrie) deriving (Show, Read, Eq)

instance NFData ResultTrie where
  rnf (ResultTrie m) = rnf m

type Result = IntMap.IntMap ResultTrie

append :: [Int] -> ResultTrie
append [] = ResultTrie IntMap.empty
append (hd:tl) = ResultTrie (IntMap.singleton hd (append tl))

traverse :: [Int] -> ResultTrie -> ResultTrie
traverse [] trie = trie
traverse t@(hd:tl) (ResultTrie n) = case IntMap.lookup hd n of
       Just n' -> ResultTrie (IntMap.insert hd (traverse tl n') n)
       Nothing -> ResultTrie (IntMap.insert hd (append tl) n)

insert :: Int -> [Int] -> Result -> Result
insert pred tuple forest = case IntMap.lookup pred forest of
       Just n -> IntMap.insert pred (traverse tuple n) forest
       Nothing -> IntMap.insert pred (append tuple) forest

tryTraverse :: [Int] -> ResultTrie -> (ResultTrie, Bool)
tryTraverse [] trie = (trie, False)
tryTraverse t@(hd:tl) (ResultTrie n) = case IntMap.lookup hd n of
       Just n' -> (ResultTrie (IntMap.insert hd newNode n), ok)
                             where (newNode, ok) = tryTraverse tl n'
       Nothing -> (ResultTrie (IntMap.insert hd (append tl) n), True)

tryTraverse' :: [Int] -> ResultTrie -> Maybe ResultTrie
tryTraverse' [] trie = Nothing
tryTraverse' t@(hd:tl) (ResultTrie n) = case IntMap.lookup hd n of
       Just n' -> do
               newNode <- tryTraverse' tl n'
               return . ResultTrie $ (IntMap.insert hd newNode n)
       Nothing -> return . ResultTrie $ (IntMap.insert hd (append tl) n)

tryInsert :: Int -> [Int] -> Result -> (Result, Bool)
tryInsert pred tuple result = case IntMap.lookup pred result of
	  Just n -> (IntMap.insert pred newTrie result, ok)
	       	    where (newTrie, ok) = tryTraverse tuple n 
	  Nothing -> (IntMap.insert pred (append tuple) result, True)

tryInsert' :: Int -> [Int] -> Result -> Maybe Result
tryInsert' pred tuple result = case IntMap.lookup pred result of
	  Just n -> do
                 newTrie <- tryTraverse' tuple n
                 return . IntMap.insert pred newTrie $ result
	  Nothing -> return . IntMap.insert pred (append tuple) $ result


justTraverse :: [Int] -> ResultTrie -> Bool
justTraverse [] trie = True
justTraverse t@(hd:tl) (ResultTrie n) = case IntMap.lookup hd n of
       Just n' -> justTraverse tl n'
       Nothing -> False

contains :: Int -> [Int] -> Result -> Bool
contains pred tuple result = case IntMap.lookup pred result of
	 Just n -> justTraverse tuple n
	 Nothing -> False

tuples :: ResultTrie -> [[Int]]
tuples (ResultTrie m)
       | size == 0 = [[]]
       | otherwise = IntMap.foldWithKey f [] m
	      	     where f key v acc = foldr (\x st -> (key:x):st) acc (tuples v)
		     	   size = IntMap.size m

getTuples :: Int -> Result -> [[Int]]
getTuples pred result = case IntMap.lookup pred result of
       Just n -> tuples n
       Nothing -> []

getTuplesWithPrefix :: Int -> [Int] -> Result -> [[Int]]
getTuplesWithPrefix pred prefix forest = case IntMap.lookup pred forest of
       Just n -> iterPrefix n prefix
       	      	 where iterPrefix n [] = tuples n
		       iterPrefix (ResultTrie n) (hd:tl) = case IntMap.lookup hd n of
		       		Just n' -> iterPrefix n' tl
				Nothing -> []
       Nothing -> []