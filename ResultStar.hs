module ResultStar where

import qualified Data.IntMap as IntMap((!),null,singleton,lookup,empty,insert,foldWithKey,IntMap,size)
import Control.Monad(liftM)
import Data.Maybe(fromJust)
import Debug.Trace(trace)

import DataStar
import EnvStar

--append :: Tuple a -> ResultTrie a
append (BothT (x:xs) l) = NodeR (IntMap.singleton x (append (BothT xs l)))
append (BothT [] l) = LeafR l
append (LeftT []) = NodeR IntMap.empty
append (LeftT (x:xs)) = NodeR (IntMap.singleton x (append (LeftT xs)))
append (RightT l) = LeafR l

--tryTraverse :: Lattice a => Tuple a -> ResultTrie a -> Maybe (ResultTrie a)
tryTraverse (BothT (x:xs) l) (NodeR n) =
  case IntMap.lookup x n of
    Just n' -> case tryTraverse (BothT xs l) n' of
                 Just newNode -> Just $ NodeR (IntMap.insert x newNode n)
                 Nothing -> Nothing
    Nothing -> Just $ NodeR (IntMap.insert x (append $ BothT xs l) n)
tryTraverse (BothT [] l) (LeafR n) = -- if l `leq` n then Nothing else Just $ LeafR l
  if n /= n' then Just . LeafR $ n' else Nothing
  where n' = l `lub` n
tryTraverse (LeftT []) _ = Nothing
tryTraverse (LeftT (x:xs)) (NodeR n) =
  case IntMap.lookup x n of
    Just n' -> case tryTraverse (LeftT xs) n' of
                 Just newNode -> Just $ NodeR (IntMap.insert x newNode n)
                 Nothing -> Nothing
    Nothing -> Just $ NodeR $ IntMap.insert x (append $ LeftT xs) n
tryTraverse (RightT l) (LeafR n) = 
  if n /= n' then Just . LeafR $ n' else Nothing
  where n' = l `lub` n
--if l `leq` n then Nothing else Just $ LeafR l

--tryInsert :: Lattice a => Int -> Tuple a -> Result a -> Maybe (Result a)
tryInsert pred tuple result = case IntMap.lookup pred result of
	  Just n -> do
               newTrie <- tryTraverse tuple n
               return $ IntMap.insert pred newTrie result
	  Nothing -> Just (IntMap.insert pred (append tuple) result)

-- | The 'check' function determines if a given tuple of atoms
-- is in the given 'ResultTrie'.
contains :: [Int] -> ResultTrie a -> Bool
contains [] _ = True
contains (x:xs) (NodeR n) = 
  case IntMap.lookup x n of
       Just n' -> contains xs n'
       Nothing -> False

-- | The 'check' function determines if a given tuple of atoms
-- is in the given 'ResultTrie'. It returns Just 'LeafR' element if
-- the tuple is in the 'ResultTrie', otherwise it returns Nothing.
check :: [Int] -> ResultTrie a -> Maybe a
check [] (LeafR l) = Just l
check [] (NodeR n) = error "check NodeR"
check (x:xs) (NodeR n) = 
      case IntMap.lookup x n of
           Just n' -> check xs n'
           Nothing -> Nothing
check _ (LeafR n) = error ("check LeafR")
{-
createEnv
  :: (Lattice a, Analysis a) =>
     Int -> Tuple a -> Args a -> (a -> a) -> Env a -> Result a -> Maybe (Env a) -}
--createEnv pre tuple args env result | trace (show (IntMap.size env)) False = undefined 
createEnv pred tuple args beta' complL env result = 
    case IntMap.lookup pred result of
	 Just n -> f tuple args n
	 Nothing -> Nothing
    where f (BothT xs _) (BothA _ (VarL y)) n = 
            case check xs n of
                 Just t -> Just $ IntMap.insert y l env
                      where l = case env IntMap.! y of
                                Just (LElem v) -> Just . LElem $ v `glb` (complL t)
                                Nothing -> Just . LElem . complL $ t
                 Nothing -> Just env
          f (BothT xs _) (BothA _ (AbsL (VarU x))) n =
            case check xs n of
                 Just t | v `leq` complL t -> Just env
                        | otherwise -> Nothing
                 Nothing -> Just env
            where v = beta' . fromAtom . fromJust $ env IntMap.! x
          f (BothT xs _) (BothA _ (AbsL (ConstU c))) n =
            case check xs n of
                 Just t | beta' c `leq` complL t -> Just env
                        | otherwise -> Nothing
                 Nothing -> Just env
          f (BothT xs _) (BothA _ (AbsL (FunU f vs))) n = undefined
          f (BothT xs _) (BothA _ (FunL f ys)) n = undefined
          f (LeftT xs) _ n = if contains xs n
                             then Nothing
                             else Just env
          f (RightT _) (RightA (VarL y)) (LeafR n) =
            Just $ IntMap.insert y l env
            where l = case env IntMap.! y of
                           Just (LElem v) -> Just . LElem $ v `glb` (complL n)
                           Nothing -> Just . LElem . complL $ n
          f (RightT _) (RightA (AbsL (VarU x))) (LeafR n)
            | v `leq` complL n = Just env
            | otherwise = Nothing
            where v = beta' . fromAtom . fromJust $ env IntMap.! x          
          f (RightT _) (RightA (AbsL (ConstU c))) (LeafR n)
            | beta' c `leq` complL n = Just env
            | otherwise = Nothing
          f (RightT _) (RightA (AbsL (FunU f vs))) (LeafR n) = undefined
          f (RightT _) (RightA (FunL f ys)) (LeafR n) = undefined

tuples :: Lattice a => ResultTrie a -> [Tuple a]
tuples (NodeR n)
       | IntMap.null n = [LeftT []] -- Shouldn't it be [EmptyT]?
       | otherwise = IntMap.foldWithKey f [] n
       where f k v acc = foldr g acc (tuples v)
                         where g (BothT xs l) acc = (BothT (k:xs) l):acc
                               g (LeftT xs) acc = (LeftT (k:xs)):acc
                               g (RightT l) acc = (BothT [k] l):acc
--tuples (LeafR l) = [BothT [] l]
tuples (LeafR l) = [RightT l]

-- | The 'f' function is an internal function, used by the 'getTuplesWithPrefix' function.
-- It matches the given prefix with all the tuples in the prefix tree,
-- then it calls the 'tuples' function to extract the suffises of matched prefixes.
--f :: Lattice a => Tuple a -> ResultTrie a -> [Tuple a]
f EmptyT t = tuples t
f (BothT (x:xs) l) (NodeR n) =
  case IntMap.lookup x n of
       Just n' -> f (BothT xs l) n'
       Nothing -> []
f (BothT [] l) (LeafR n) = if l `leq` n then [EmptyT] else [RightT n]
f (LeftT []) n = tuples n
f (LeftT (x:xs)) (NodeR n) =
  case IntMap.lookup x n of
       Just n' -> f (LeftT xs) n'
       Nothing -> []
f (RightT l) (LeafR n) = if l `leq` n then [EmptyT] else [RightT n]

-- | The 'getTuplesWithPrefix' function for a given predicate extracts suffixes of all tuples
-- with a given prefix from the result.
--getTuplesWithPrefix :: Lattice a => Int -> Tuple a -> Result a -> [Tuple a]
getTuplesWithPrefix pred prefix _ | trace ("getTuplesWithPrefix prefix=" ++ show prefix) False = undefined
getTuplesWithPrefix pred prefix result = 
                    case IntMap.lookup pred result of
                         Just n -> f prefix n
                         Nothing -> []
{-
doit :: Lattice a => ArgL a -> a -> Env a -> [(Env a, [Tuple a])]
doit (VarL x) n env = [(IntMap.insert x (Just $ LElem n) env, [EmptyT])]
doit (AbsL (ConstU a)) n env = if beta a `leq` n 
                                        then [(env, [EmptyT])]
                                        else [(env, [])]
doit (AbsL (VarU x)) n env = 
             case env IntMap.! x of
                  Just (Atom v) | beta v `leq` n -> [(env, [EmptyT])]
                         | otherwise -> [(env, [])]
                  Nothing -> map (\e -> (e, [EmptyT])) envs
                             where envs = map newEnv (betaInv n)
                                   newEnv c = IntMap.insert x (Just $ Atom c) env
doit (AbsL (FunU f xs)) n env = undefined
doit (FunL f xs) n env = undefined

g :: Lattice a => Tuple a -> ArgL a -> Env a -> ResultTrie a -> [(Env a, [Tuple a])]
g EmptyT _ env t = [(env, tuples t)]
g (BothT (x:xs) l) argL env (NodeR n) =
  case IntMap.lookup x n of
       Just n' -> g (BothT xs l) argL env n'
       Nothing -> [(env, [])]
g (BothT [] l) argL env (LeafR n) = if l `leq` n then [(env, [EmptyT])] else doit argL n env
g (LeftT []) _ env n = [(env, tuples n)]
g (LeftT (x:xs)) argL env (NodeR n) =
  case IntMap.lookup x n of
       Just n' -> g (LeftT xs) argL env n'
       Nothing -> [(env, [])]
g (RightT l) argL env (LeafR n) = if l `leq` n then [(env, [EmptyT])] else doit argL n env

getTuplesWithPrefix' :: Lattice a => Int -> Tuple a -> ArgL a -> Env a -> Result a -> [(Env a, [Tuple a])]
getTuplesWithPrefix' pred prefix argL env result = 
            case IntMap.lookup pred result of
                 Just n -> g prefix argL env n
                 Nothing -> [(env, [])]
-}

getTuples :: Lattice a => Int -> Result a -> [Tuple a]
getTuples pred result = 
          case IntMap.lookup pred result of
               Just n -> tuples n
               Nothing -> []