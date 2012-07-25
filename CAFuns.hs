module CAFuns where

import qualified Data.IntSet as IntSet
import Data.IntSet(IntSet)

import DataStar

trans_us us = 

trans_u c@(ConstU u) = (IntSet.empty, c)
trans_u v@(VarU x) = (IntSet.empty, v)
trans_u vid (FunU f us) =
        where (s, us') = trans_us us