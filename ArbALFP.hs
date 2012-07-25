{-# Language FlexibleInstances #-}
module ArbALFP where

import Test.QuickCheck

import Data.Ord
import Control.Monad(mapM,replicateM,liftM,foldM)
import qualified Data.List as List(partition,length,foldl',null,maximum,map,maximumBy,reverse,zip,and,foldr)
import qualified Data.Map as Map(fromList,insert,empty,fold,elems,size,foldrWithKey)
import Data
import ALFPLogic

instance Arbitrary (SolverData Clause) where
  arbitrary = sized aSolverData
  shrink sd = List.map (\c -> sd { cls = c }) xs 
              where cl = cls sd
                    univ = Map.elems (Data.atoms sd)
                    xs = shrinkClause cl univ

aSolverData size = do
    bitsNo <- choose (1,2::Int)
    let statesNo = 2^bitsNo
        atms = [0..statesNo-1]
    layerNo <- choose (1,10::Int)
    let arb = ArbData { aVars = []
                      , aAsrt = []
                      , aNqry = []
                      , aAtms = atms
                      , aIdx = 0 }
        f (lrs, acc) idx = do
          pred <- newDefPred $ predCount acc
          (l, acc') <- aClause size (addAsrt pred acc)
          return (l:lrs, moveToNqry acc')
    (acls,arb') <- foldM f ([],arb) [1..layerNo]
    let prds = (aAsrt arb') ++ (aNqry arb')
        outCls = case List.reverse acls of
                      [] -> TT
                      xs -> foldr1 And xs
        pmap = List.foldl' (\acc pd -> Map.insert (show . pid $ pd) pd acc) Map.empty prds
        amap = List.foldl' (\acc x -> Map.insert (show x) x acc) Map.empty atms
        maxArty = parity . List.maximumBy (comparing parity) $ prds
        maxDepth = List.maximum. List.map clauseDepth $ acls
        sd = SolverData { cls = List.reverse acls
                      , predicates = pmap
                      , atoms = amap
                      , maxPredicateArity = maxArty
                      , maxClauseDepth = maxDepth }
    if isStratified' sd 
    then return sd { cls = outCls }
    else error ("Cls is not stratified." ++ (show outCls))

data ArbData = ArbData { aVars :: [Int]
                       , aAsrt :: [PredData]
                       , aNqry :: [PredData]
                       , aAtms :: [Int] 
                       , aIdx :: Int }

nextMIdx ad = let n = aIdx ad
              in (n, ad {aIdx = n+1})
newAsrt ad = do
        pred <- newDefPred $ predCount ad
        return $ addAsrt pred ad
moveToNqry ad = ad { aNqry = (aNqry ad) ++ (aAsrt ad), aAsrt = [] }
addAsrt pd ad = ad { aAsrt = pd : (aAsrt ad) }
predCount ad = (List.length . aAsrt $ ad) + (List.length . aNqry $ ad)
addVar x ad = let xs = aVars ad
                in ad { aVars = x:xs }
deleteVar ad = let (x:xs) = aVars ad
               in ad { aVars = xs }

newDefPred n = do
        x <- choose (1,3)
        return PredData { pid = n
                        , parity = x
                        , ptype = DefPred }       

aClause size arbd
           | size > 0 = do
             k <- choose (0,3::Int)
             case k of
                  0 -> aAssert arbd
                  1 -> aAnd (size-1) arbd
                  2 -> aImply (size-1) arbd
                  3 -> if (List.length . aVars $ arbd) > 3 then aAssert arbd else aForall (size-1) arbd
--                  3 -> aForall (size-1) arbd
           | otherwise = aAssert arbd

aAssert arbd = do
        (p, ad) <- aPred arbd
        return (p, ad)

aPred arbd = do
      k <- choose (0,1::Int)
      (pred, ad) 
             <- case k of
                     0 -> do
                       p <- newDefPred $ predCount arbd
                       return (p, addAsrt p arbd)
                     1 -> do  
                       p <- elements . aAsrt $ arbd
                       return (p, arbd)
      args <- replicateM (parity pred) (aArg ad)
      return (Assertion (pid pred) $ args, ad)

aAnd size arbd = do
     (cl1, arbd1) <- aClause (size-1) arbd
     (cl2, arbd2) <- aClause (size-1) arbd1
     return (And cl1 cl2, arbd2)

aImply size arbd = do
          (cl, arbd1) <- aClause (size-1) arbd
          (pre, arbd2) <- aPre (size-1) arbd1
          return (Imply pre cl, arbd2)

aForall size arbd = do
        let x = List.length . aVars $ arbd
        (cl, arbd1) <- aClause (size-1) (addVar x arbd)
        return (Forall x cl, deleteVar arbd1)


aPre size arbd = do
     k <- choose (0,7::Int)
     case k of
       0 -> aQuery arbd
       1 | List.null . aNqry $ arbd -> aQuery arbd
         | otherwise -> aNegQuery arbd
       2 -> aAndPre (size-1) arbd
       3 -> aOrPre (size-1) arbd
--       4 -> aExistsPre (size-1) arbd
--       5 -> aForallPre (size-1) arbd
       4 -> if (List.length . aVars $ arbd) > 3 then aQuery arbd else aExistsPre (size-1) arbd
       5 -> if (List.length . aVars $ arbd) > 3 then aQuery arbd else aForallPre (size-1) arbd
       6 -> return (TruePre, arbd)
       7 -> return (FalsePre, arbd)

aQuery arbd = do
      let preds = (aNqry arbd)
      if null preds 
        then return (TruePre, arbd)
      else do                   
        pred <- elements . aNqry $ arbd
        args <- replicateM (parity pred) (aArg arbd)
        return (Query (pid pred) args, arbd)

aNegQuery arbd = do
       pred <- elements . aNqry $ arbd
       args <- replicateM (parity pred) (aArg arbd)
       return (NegQuery (pid pred) args, arbd)

aAndPre size arbd = do
        (pre1, arbd1) <- aPre (size-1) arbd
        (pre2, arbd2) <- aPre (size-1) arbd1
        return (AndPre pre1 pre2, arbd2)

aOrPre size arbd = do
       (pre1, arbd1) <- aPre (size-1) arbd
       (pre2, arbd2) <- aPre (size-1) arbd1
       let (n, arbd3) = nextMIdx arbd2
       return (Or pre1 pre2 n, arbd3)

aForallPre size arbd = do
           let x = List.length . aVars $ arbd
           (pre, arbd1) <- aPre (size-1) (addVar x arbd)
           return (ForallPre x pre, deleteVar arbd1)

aExistsPre size arbd = do
           let x = List.length . aVars $ arbd
           (pre, arbd1) <- aPre (size-1) (addVar x arbd)
           let (n, arbd2) = nextMIdx arbd1
           return (Exist x pre n, deleteVar arbd2)

aArg :: ArbData -> Gen Argument
aArg arbd = do
     let vs = aVars arbd
     k <- if List.null vs then return 0 else choose (0,1::Int)
     case k of
       0 -> do
           a <- elements . aAtms $ arbd
           return $ Const a
       otherwise -> do
           x <- elements . aVars $ arbd
           return $ Var x

shrinkPre (AndPre pre1 pre2) _ = [pre1,pre2,TruePre,FalsePre]
shrinkPre (Or pre1 pre2 _) _ = [pre1,pre2,TruePre,FalsePre]
shrinkPre (Exist x pre _) univ = 
  List.map (\a -> replaceArgInPre pre (Var x) (Const a)) univ
shrinkPre (ForallPre x pre) univ =
  List.map (\a -> replaceArgInPre pre (Var x) (Const a)) univ
shrinkPre (Query p args) _ = [TruePre,FalsePre]
shrinkPre (NegQuery p args) _ = [TruePre,FalsePre]
shrinkPre (Eq _ _) _ = [TruePre,FalsePre]
shrinkPre (Neq _ _) _ = [TruePre,FalsePre]
shrinkPre _ _ = []

shrinkClause (Assertion p args) _ = [TT]
shrinkClause (And cl1 cl2) _ = [cl1,cl2, And cl1 TT]
shrinkClause (Imply pre cl) _ = [Imply TruePre cl, Imply FalsePre cl, cl]
shrinkClause (Forall x cl) univ = 
  List.map (\a -> replaceArgInClause cl (Var x) (Const a)) univ
shrinkClause TT _ = []