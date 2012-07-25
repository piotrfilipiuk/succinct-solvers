module PreProcessor where

import Data

toWellFormedFormula (Predicate p args) = (Predicate p args)
toWellFormedFormula (NegPredicate p args) = (NegPredicate p args)
toWellFormedFormula (Wedge f1 f2) = undefined
--toWellFormedFormula

{-
toDNF (Predicate p args) = (Predicate p args)
toDNF (NegPredicate p args) = (NegPredicate p args)
toDNF (Wedge f1 f2) = distr dnf1 dnf2
                      where dnf1 = toDNF f1
                            dnf2 = toDNF f2
toDNF (Vee f1 f2) = Vee dnf1 dnf2
                    where dnf1 = toDNF f1
                          dnf2 = toDNF f2
toDNF _ = error "toDNF: Not implemented."
-}