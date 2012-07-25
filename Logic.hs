module Main where

import Test.QuickCheck
import Control.Monad

data Formula
       = Pos Int
       | Neg Int
       | Wedge Formula Formula
       | Vee Formula Formula
         deriving (Read, Show)

instance Arbitrary Formula where
  arbitrary = sized f
              where f 0 = oneof [liftM Pos arbitrary, liftM Neg arbitrary]
                    f n = frequency [(1, liftM Pos arbitrary), (1, liftM Neg arbitrary), (5, liftM2 Wedge (f (n `div` 2)) (f (n `div` 2))), (5, liftM2 Vee (f (n `div` 2)) (f (n `div` 2)))]
--                    f n = oneof [liftM Pos arbitrary, liftM Neg arbitrary, liftM2 Wedge (f (n - 10)) (f (n - 10)), liftM2 Vee (f (n - 10)) (f (n - 10))]
  
toDNF :: Formula -> Formula
toDNF (Pos p) = (Pos p)
toDNF (Neg p) = (Neg p)
toDNF (Wedge f1 f2) = distr dnf1 dnf2
                      where dnf1 = toDNF f1
                            dnf2 = toDNF f2
toDNF (Vee f1 f2) = Vee dnf1 dnf2
                    where dnf1 = toDNF f1
                          dnf2 = toDNF f2
--toDNF _ = error "toDNF: Not implemented."

distr :: Formula -> Formula -> Formula
distr (Vee f1 f2) dnf2 = Vee (distr f1 dnf2) (distr f2 dnf2)
distr dnf1 (Vee f1 f2) = Vee (distr dnf1 f1) (distr dnf1 f2)
distr f1 f2 = Wedge f1 f2

isDNF (Pos p) = True
isDNF (Neg p) = True
isDNF (Vee f1 f2) = isDNF f1 && isDNF f2
isDNF (Wedge f1 f2) = isWedgeOrSimple f1 && isWedgeOrSimple f2

isWedgeOrSimple (Pos p) = True
isWedgeOrSimple (Neg p) = True
isWedgeOrSimple (Wedge f1 f2) = isWedgeOrSimple f1 && isWedgeOrSimple f2
isWedgeOrSimple (Vee f1 f2) = False

main = do
  verboseCheck (isDNF . toDNF :: Formula -> Bool)