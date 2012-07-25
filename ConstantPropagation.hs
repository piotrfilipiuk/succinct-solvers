module ConstantPropagation where

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.IntSet as IntSet

import DataStar
{-
data ZCP
     = ZCP Int
     | TopZCP

instance Lattice ZCP where
         leq _ TopZCP = True
         leq TopZCP _ = False
         leq z1 z2 = z1 == z2
         lub 

data StateCP
     = BottomStateCP
     | StateCP IntMap.IntMap ZCP

instance Lattice StateCP where
         leq BottomStateCP _ = True
         leq _ BottomStateCP = False
         leq s1 s2 = 
-}

type StateCP = IntMap.IntMap Int

instance Eq a => Lattice (IntMap.IntMap a) where
         leq s1 s2 = f ds
             where ds1 = IntMap.keysSet s1
                   ds2 = IntMap.keysSet s2
                   ds = IntSet.toList . IntSet.union ds1 $ ds2
                   f [] = True
                   f (x:xs) = 
                     case (v1, v2) of
                       (_, Nothing) -> f xs
                       (Just n1, Just n2) | n1 == n2 -> f xs
                       _ -> False
                     where v1 = IntMap.lookup x s1
                           v2 = IntMap.lookup x s2
         lub s1 s2 = foldl f IntMap.empty ds
             where ds1 = IntMap.keysSet s1
                   ds2 = IntMap.keysSet s2
                   ds = IntSet.toList . IntSet.union ds1 $ ds2
                   f acc x = 
                     case (v1, v2) of
                       (Just n1, Just n2) | n1 == n2 -> IntMap.insert x n1 acc
                       _ -> acc
                     where v1 = IntMap.lookup x s1
                           v2 = IntMap.lookup x s2
         glb = undefined
         bot = undefined
         top _ = IntMap.empty
         compl = undefined

instance Analysis (IntMap.IntMap a) where
         beta _ = IntMap.empty
         betaInv _ = error "betaInv for CP should not be used."

functionsCP = Map.singleton "f" (FunctL (\[] -> IntMap.singleton 1 666))
