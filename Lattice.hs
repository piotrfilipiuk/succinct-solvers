module Lattice where

import qualified Data.IntMap as IntMap

class Lattice a where
  lub :: a -> a -> a
--  glb :: a -> a -> a
--  bot :: a
--  top :: a
--  leq :: a -> a -> Bool

data Val = Top 
         | Value Int

type Sigma = IntMap.IntMap Val
type CP = Maybe Sigma

instance Lattice Val where
  lub Top _ = Top
  lub _ Top = Top
  lub (Value z1) (Value z2)
    | z1 == z2 = Value z1
    | otherwise = Top

instance (Lattice a) => Lattice (IntMap.IntMap a)  where
  --Here it is assumed that both sigmas have the same set of keys (variables).
  lub sigma1 sigma2 = IntMap.mapWithKey f sigma1
                    where f key elem = lub v elem
                                       where v = sigma2 IntMap.! key

instance (Lattice a) => Lattice (Maybe a) where
  lub Nothing (Just s) = Just s
  lub (Just s) Nothing = Just s
  lub (Just s1) (Just s2) = Just (lub s1 s2)