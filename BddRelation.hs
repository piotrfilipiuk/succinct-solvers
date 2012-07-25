module BddRelation where

import qualified Data.IntMap as IntMap

import Control.Monad
import Foreign.C.Types
import Bdd
import Fdd
import Data

isEmpty :: CInt -> Int -> Bool
isEmpty bdd n = (bdd == 0) && (n /= 0)


complement :: CInt -> IO (CInt)
complement bdd = bdd_not bdd

{-
product :: (CInt, Int) -> (CInt, Int) -> IO (CInt)
product (bdd1, n1) (bdd2, n2)
  | isEmpty bdd1 n1 || isEmpty bdd2 n2 = return 0
  | bdd1 == 0 = return bdd2
  | bdd2 == 0 = return bdd1
  | otherwise = bdd_and bdd1 bdd2
-}

product' :: CInt -> CInt -> IO CInt
product' bdd1 bdd2
  | bdd1 == 0 || bdd2 == 0 = return 0
  | otherwise = bdd_and bdd1 bdd2

projectOut xs bdd = do
  b <- bdd_addref bdd
  foldM f b xs
  where f acc x = do
          bddi <- fdd_ithset (fromIntegral x)
          result <- bdd_exist acc bddi
          bdd_delref bddi
          bdd_delref acc
          return result
  
union :: CInt -> CInt -> IO (CInt)
union = bdd_or

intersection :: CInt -> CInt -> IO (CInt)
intersection = bdd_and

{-
isProperSubset :: CInt -> CInt -> IO Bool
isProperSubset bdd1 bdd2 = do
  intersection <- bdd_and bdd1 bdd2
  return ((intersection == bdd1) && (bdd1 /= bdd2))
-}

--select (Var x) idx bdd = fdd_equals (fromIntegral x) idx >>= (bdd_and bdd)
--select (Const c) idx bdd = fdd_ithvar idx (fromIntegral c) >>= (bdd_and bdd)

select (Var x) idx bdd = do
  bddEq <- fdd_equals (fromIntegral x) idx 
  result <- bdd_and bdd bddEq
  bdd_delref bddEq
  return result
  
select (Const c) idx bdd = do
  bddVar <- fdd_ithvar idx (fromIntegral c) 
  result <- bdd_and bdd bddVar
  bdd_delref bddVar
  return result

select' (Const c1) (Const c2) bdd
  | c1 == c2 = bdd_addref bdd
  | otherwise = bddfalse
select' (Var x) (Const c) bdd = do
  bddVar <- fdd_ithvar (fromIntegral x) (fromIntegral c)
  result <- bdd_and bdd bddVar
  bdd_delref bddVar
  return result
select' (Const c) (Var x) bdd = select' (Var x) (Const c) bdd
select' (Var x1) (Var x2) bdd = do 
  bddEq <- fdd_equals (fromIntegral x1) (fromIntegral x2)
  result <- bdd_and bdd bddEq
  bdd_delref bddEq
  return result

exist x bdd = do
  bddSet <- fdd_ithset (fromIntegral x)
  result <- bdd_exist bdd bddSet
  bdd_delref bddSet
  return result
  
forall x bdd = do
  bddSet <- fdd_ithset (fromIntegral x)
  result <- bdd_forall bdd bddSet
  bdd_delref bddSet
  return result