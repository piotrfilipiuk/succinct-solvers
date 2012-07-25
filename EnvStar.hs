module EnvStar where

import qualified Data.IntMap as IntMap(IntMap,(!),insert)
--import qualified Data.List as List(length)
import Data.Maybe(isNothing,fromJust)
import Control.Monad(foldM)
import qualified Control.Exception as Exc
import qualified Data.IntSet as IntSet
import Debug.Trace(trace)

import DataStar

--unify :: (Lattice a, Analysis a) => Args a -> Tuple a -> Env a -> [Env a]
unify _ a t env | trace ("unify: args=" ++ show a ++ "tuple=" ++ show t) False = undefined
unify _ EmptyA EmptyT env = [env]
unify _ (BothA [] (VarL x)) (BothT [] t) env =
      case env IntMap.! x of
           Nothing -> [IntMap.insert x (Just $ LElem t) env]
           Just (LElem l) | l `leq` t -> [env]
                          | otherwise -> if l' /= bot 
                                         then [IntMap.insert x (Just $ LElem l') env] 
                                         else []
                          where l' = l `glb` t
unify beta' (BothA [] (AbsL (ConstU a))) (BothT [] t) env
      | beta' a `leq` t = [env]
      | otherwise = []
unify beta' (BothA [] (AbsL (VarU x))) (BothT [] t) env =
      case env IntMap.! x of
           Nothing -> map (\v -> IntMap.insert x (Just . Atom $ v) env) (betaInv t)
           Just (Atom a) | beta' a `leq` t -> [env]
                         | otherwise -> []
unify _ (BothA [] (AbsL (FunU f xs))) (BothT [] t) env = undefined
unify _ (BothA [] (FunL f xs)) (BothT [] t) env = undefined
unify beta' (BothA ((ConstU x):xs) a) (BothT (y:ys) t) env
      | x == y = unify beta' (BothA xs a) (BothT ys t) env
      | otherwise = []
unify beta' (BothA ((VarU x):xs) a) (BothT (y:ys) t) env
      | isNothing v = unify beta' (BothA xs a) (BothT ys t) env'
      | (fromAtom . fromJust $ v) == y = unify beta' (BothA xs a) (BothT ys t) env
      | otherwise = []
      where v = env IntMap.! x
            env' = IntMap.insert x (Just $ Atom y) env
unify _ (BothA ((FunU f us):xs) a) (BothT (y:ys) t) env = undefined
unify _ (BothA _ _) _ _ = error "Tuple is not BothT ..."
unify _ (LeftA []) (LeftT []) env = [env]
unify beta' (LeftA ((ConstU x):xs)) (LeftT (y:ys)) env
      | x == y = unify beta' (LeftA xs) (LeftT ys) env
      | otherwise = []
unify beta' (LeftA ((VarU x):xs)) (LeftT (y:ys)) env
      | isNothing v = unify beta' (LeftA xs) (LeftT ys) env'
      | (fromAtom . fromJust $ v) == y = unify beta' (LeftA xs) (LeftT ys) env
      | otherwise = []
      where v = env IntMap.! x
            env' = IntMap.insert x (Just $ Atom y) env
unify _ (LeftA ((FunU f vs):xs)) (LeftT (y:ys)) env = error "unify for FunU not implemented."
unify _ (LeftA _) (LeftT _) env = error "Lists length do not match."
unify _ (LeftA _) _ env = error "Tuple is not LeftT."
unify _ (RightA (VarL x)) (RightT t) env =
      case env IntMap.! x of
           Nothing -> [IntMap.insert x (Just $ LElem t) env]
           Just (LElem l) | l `leq` t -> [env]
                          | otherwise -> if l' /= bot 
                                         then [IntMap.insert x (Just $ LElem l') env] 
                                         else []
                          where l' = l `glb` t
unify _ (RightA (FunL f xs)) (RightT t) env = undefined
unify beta' (RightA (AbsL (ConstU a))) (RightT t) env
      | beta' a `leq` t = [env]
      | otherwise = []
unify beta' (RightA (AbsL (VarU x))) (RightT t) env =
      case env IntMap.! x of
           Nothing -> map (\v -> IntMap.insert x (Just . Atom $ v) env) (betaInv t)
           Just (Atom a) | beta' a `leq` t -> [env]
                         | otherwise -> []
unify _ (RightA (AbsL (FunU f xs))) (RightT t) env = undefined
unify _ (RightA _) (BothT _ _) _ = error "unify (RightA _) (BothT _ _)"
unify _ EmptyA (LeftT _) _ = error "Env.unify EmptyA (LeftT _)"


getVarsU :: [ArgU] -> Env a -> [Int]
getVarsU xs env = foldr f [] xs
         where f (VarU x) acc =
                 case env IntMap.! x of
                      Just _ -> acc
                      Nothing -> x:acc
               f (FunU f ys) acc = (getVarsU ys env) ++ acc
               f (ConstU _) acc = acc
               
getVarsL :: ArgL a -> Env a -> ([Int], [Int])
getVarsL a env | trace ("getVarsL: " ++ show a) False = undefined
getVarsL (VarL x) env =
         case env IntMap.! x of
              Just _ -> ([], [])
              Nothing -> ([], [x])
getVarsL (AbsL (ConstU _)) _ = ([], [])
getVarsL (AbsL (VarU x)) env =
         case env IntMap.! x of
              Just _ -> ([], [])
              Nothing -> ([x], [])
getVarsL (AbsL (FunU f xs)) env = (getVarsU xs env, [])
getVarsL (FunL f xs) env = foldr g ([],[]) xs
                           where g x (xs,ys) = let (xs',ys') = getVarsL x env
                                               in (xs'++xs, ys'++ys)

--- Returns a tuple (xs,Ys) where:
--- xs - not evaluated variables from X (for atoms)
--- Ys - not evaluated variables from Y (for lattice elements)
getNotEvaluatedVariables :: Args a -> Env a -> ([Int],[Int])
getNotEvaluatedVariables (BothA xs l) env = (vs ++ getVarsU xs env, ys)
                                            where (vs,ys) = getVarsL l env
getNotEvaluatedVariables (LeftA xs) env = (getVarsU xs env, [])
getNotEvaluatedVariables (RightA l) env = getVarsL l env

-- All variables should be evaluated.
evalU :: [ArgU] -> Env a -> [Int]
evalU args env = foldr f [] args
	 where f (VarU x) acc = (fromAtom . fromJust $ env IntMap.! x) : acc
	       f (ConstU a) acc = a : acc
               f (FunU f xs) acc = (f $ evalU xs env):acc

-- All variables should be evaluated.
--evalL :: (Lattice a, Analysis a) => Env a -> ArgL a -> a
evalL _ env (VarL x) = fromLElem . fromJust $ env IntMap.! x
evalL beta' env (AbsL (ConstU n)) = beta' n
evalL beta' env (AbsL (VarU x)) = beta' . fromAtom . fromJust $ env IntMap.! x
evalL beta' env (AbsL (FunU f xs)) = beta' . f . evalU xs $ env
evalL beta' env (FunL f xs) = f $ map (evalL beta' env) xs

--evaluate :: (Lattice a, Analysis a) => Args a -> Env a -> Tuple a
evaluate beta' (BothA xs l) env = BothT vs p
                            where vs = evalU xs env
                                  p = evalL beta' env l
evaluate _ (LeftA xs) env = LeftT $ evalU xs env
evaluate beta' (RightA l) env = RightT $ evalL beta' env l

--createEnvList :: ([Int], [Int]) -> [Int] -> a -> Env a -> [Env a]
createEnvList (xs, ys) univ top env = foldr h envs ys
	      where envs = foldr f [env] xs
                    f var acc = concat $ map (g var) acc
	      	    g x e = map (\a -> IntMap.insert x (Just . Atom $ a) e) univ
                    h var acc = map (k var) acc
                    k x e = IntMap.insert x (Just . LElem $ top) e
                    
--createTuples :: (Lattice a, Analysis a) => [Env a] -> Args a -> [Tuple a]
createTuples beta' envList args = Exc.assert ((length tuples) == (length envList)) tuples --foldr f [] envList
	     where f env acc = (evaluate beta' args env) : acc
                   tuples = foldr f [] envList
{-
unifiable :: 
  (Lattice a, Analysis a) => 
  ([Int], [Int]) -> Args a -> [Int] -> a -> Env a -> [Tuple a] -}
unifiable vars args univ beta' top env = createTuples beta' (createEnvList vars univ top env) args
{-
--tryEvalU :: [ArgU] -> Env a -> Maybe [Int]
tryEvalU args env = foldM f [] args
	 where f acc (VarU x) = 
                 case env IntMap.! x of 
                   Just (Atom n) -> Just $ n : acc
                   Nothing -> Nothing
	       f acc (ConstU n) = Just $ n : acc
               f acc (FunU f xs) =
                 case tryEvalU xs env of
                   Just vs -> Just $ (f vs) : acc
                   Nothing -> Nothing
               
--tryEvalL :: (Lattice a, Analysis a) => Env a -> ArgL a -> Maybe (EnvVal a)
tryEvalL env (VarL x) = env IntMap.! x
tryEvalL env (AbsL (ConstU n)) = Just . LElem . beta $ n
tryEvalL env (AbsL (VarU x)) =
  case env IntMap.! x of
    Just (Atom n) -> Just . LElem . beta $ n
    Nothing -> Nothing
tryEvalL env (AbsL (FunU f xs)) =
  case tryEvalU xs env of
    Just vs -> Just . LElem . beta . f $ vs
    Nothing -> Nothing
tryEvalL env (FunL f xs) =
  case foldM g [] xs of
    Just vs -> Just . LElem . f $ vs
    Nothing -> Nothing
  where g acc x = case tryEvalL env x of
          Just (LElem l) -> Just $ l : acc
          Nothing -> Nothing
-}
--split :: (Lattice a, Analysis a) => [Int] -> Args a -> Env a -> (Tuple a, Args a)
split acc EmptyA _ = (EmptyT, EmptyA)
split acc (BothA [] l) env = (LeftT $ reverse acc, RightA l)
split acc (BothA ((ConstU n):tl) l) env = split (n : acc) (BothA tl l) env
split acc (BothA xs@((VarU x):tl) l) env =
  case env IntMap.! x of
	Just (Atom v) -> split (v : acc) (BothA tl l) env
        Nothing -> (LeftT $ reverse acc, BothA xs l)
--split acc (LeftA []) env = (LeftT $ reverse acc, EmptyA)
split acc (LeftA []) env = (LeftT $ reverse acc, LeftA [])
split acc (LeftA ((ConstU n):tl)) env = split (n : acc) (LeftA tl) env
split acc (LeftA xs@((VarU x):tl)) env =
  case env IntMap.! x of
	Just (Atom v) -> split (v : acc) (LeftA tl) env
        Nothing -> (LeftT $ reverse acc, LeftA xs)
split acc (RightA l) env = (EmptyT, RightA l)

--splitArgs :: (Lattice a, Analysis a) => Args a -> Env a -> (Tuple a, Args a)
splitArgs args env = split [] args env