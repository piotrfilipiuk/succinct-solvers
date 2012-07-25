module EnvTest where

import Test.QuickCheck
--import Test.QuickCheck.Gen
import Control.Monad
import Env
import Data
import qualified Data.IntMap as IntMap
import Data.List(nub)

instance Arbitrary Argument where
  arbitrary = oneof [liftM Var arbitrary, liftM Const arbitrary]

instance Arbitrary IntMap.IntMap (Maybe Int) where
  arbitrary = 

--Test for getNotEvaluatedVariables :: [Argument] -> IntMap.IntMap (Maybe Int) -> [Int]
test_getNotEvaluatedVariables args env = xs1 == xs2
                                         where xs1 = nub . getNotEvaluatedVariables args $ env
                                               xs2 = getNotEvaluatedVariables' args env

runtests = do
  quickCheck (test_getNotEvaluatedVariables :: [Argument] -> IntMap.IntMap (Maybe Int) -> Bool)