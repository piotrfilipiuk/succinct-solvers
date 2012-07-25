{-# Language GADTs,FlexibleInstances #-}
module ArbLFP where

import LFPLogic
import Data
import Data.Ord

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import qualified Data.Map as Map(fromList,insert,empty,elems)
import qualified Data.List as List(partition,length,foldl',null,maximum,map,maximumBy,reverse)
import Control.Monad(mapM,replicateM,liftM,foldM)
import Debug.Trace(trace)

import ALFPLogic(replaceArgInPre,replaceArgInArgs)
import ArbALFP(shrinkPre,ArbData(..),moveToNqry,aAsrt,newDefPred,predCount,addAsrt,aArg,aPre,addVar,deleteVar)

instance Arbitrary (SolverData [LFPLayer]) where
  arbitrary = sized aSolverData
{-  shrink sd = List.map (\c -> sd { cls = c }) xs 
              where cl = cls sd
                    univ = Map.elems (Data.atoms sd)
                    xs = shrinkList (\c -> shrinkLayer c univ) cl
-}
shrinkLayer (DefLayer l) univ = List.map DefLayer . shrinkDefClause l $ univ
shrinkLayer (ConLayer l) univ = List.map ConLayer . shrinkConClause l $ univ


shrinkDefClause (AndClause cl1 cl2) univ = [ AndClause c1 c2 | c1 <- cl1', c2 <- cl2' ]
                where cl1' = shrinkDefClause cl1 univ
                      cl2' = shrinkDefClause cl2 univ
shrinkDefClause (ForAll x cl) univ = acl ++ scl
                where pcl = List.map (\a -> replaceArgInDefClause cl (Var x) (Const a)) univ
                      rcl = List.map (\c -> shrinkDefClause c univ) pcl
                      scl = shrinkDefClause cl univ
                      acl = List.map (\c -> ForAll x c) scl
shrinkDefClause (DefImply pre p) univ = List.map (\pre' -> DefImply pre' p) pres'
                where pres' = shrinkPre pre univ
shrinkDefClause asrt _ = [asrt]

shrinkConClause (AndClause cl1 cl2) univ = [ AndClause c1 c2 | c1 <- cl1', c2 <- cl2' ]
                where cl1' = shrinkConClause cl1 univ
                      cl2' = shrinkConClause cl2 univ
shrinkConClause (ForAll x cl) univ = acl ++ scl
                where pcl = List.map (\a -> replaceArgInConClause cl (Var x) (Const a)) univ
                      rcl = List.map (\c -> shrinkConClause c univ) pcl
                      scl = shrinkConClause cl univ
                      acl = List.map (\c -> ForAll x c) scl
shrinkConClause (ConImply p pre) univ =  List.map (\pre' -> ConImply p pre') pres'
                where pres' = shrinkPre pre univ
shrinkConClause constr _ = [constr]

aSolverData size = do
    bitsNo <- choose (1,5::Int)
    let statesNo = 2^bitsNo
        atms = [0..statesNo-1]
    layerNo <- choose (1,10::Int)
    let arb = ArbData { aVars = []
                      , aAsrt = []
                      , aNqry = []
                      , aAtms = atms
                      , aIdx = 0 }
        aLf (lrs, acc) idx = do
            pKind <- elements [DefPred, ConPred]
            case pKind of
                 DefPred -> do
                         (l, acc') <- aDefClause size acc
                         return ((DefLayer l):lrs, moveToNqry acc')
                 ConPred -> do
                         (l, acc') <- aConClause size acc
                         return ((ConLayer l):lrs, moveToNqry acc')
    (acls,arb') <- foldM aLf ([],arb) [1..layerNo]
    let prds = (aAsrt arb') ++ (aNqry arb')
        pmap = List.foldl' (\acc pd -> Map.insert (show . pid $ pd) pd acc) Map.empty prds
        amap = List.foldl' (\acc x -> Map.insert (show x) x acc) Map.empty atms
        maxArty = parity . List.maximumBy (comparing parity) $ prds
        maxDepth = List.maximum. List.map (depthClause . getClause) $ acls
        sd = SolverData { cls = List.reverse acls
                      , predicates = pmap
                      , atoms = amap
                      , maxPredicateArity = maxArty
                      , maxClauseDepth = maxDepth }
    if isStratified' sd then return sd else error "Cls is not stratified."

newConPred n = do
           x <- choose (1,3)
           return PredData { pid = n
                           , parity = x
                           , ptype = ConPred }       

aConClause size arbd
           | size > 0 = do
             k <- choose (0,3::Int)
             case k of
                  0 -> aConstr arbd
                  1 -> aConAnd (size-1) arbd
                  2 -> aConImply (size-1) arbd
                  3 -> aConForall (size-1) arbd
           | otherwise = aConstr arbd

aDefClause size arbd
           | size > 0 = do
             k <- choose (0,3::Int)
             case k of
                  0 -> aAssert arbd
                  1 -> aDefAnd (size-1) arbd
                  2 -> aDefImply (size-1) arbd
                  3 -> aDefForall (size-1) arbd
           | otherwise = aAssert arbd

aAssert arbd = do
        (p, ad) <- aDefPred arbd
        return (Assert p, ad)

aConstr arbd = do
        (p, ad) <- aConPred arbd
        return (Constr p, ad)

--aPred :: ArbData -> Gen Pred
aDefPred arbd = do
  let as = aAsrt arbd
      f = do
          p <- newDefPred $ predCount arbd
          return (p, addAsrt p arbd)
      g = do
          p <- elements as
          return (p, arbd)
      h = do
          k <- arbitrary
          if k then f else g
  (pred, ad) <- if null as then f else h
  args <- replicateM (parity pred) (aArg ad)
  return (Pred (pid pred) $ args, ad)

aConPred arbd = do
  let as = aAsrt arbd
      f = do
          p <- newConPred $ predCount arbd
          return (p, addAsrt p arbd)
      g = do
          p <- elements as
          return (p, arbd)
      h = do
          k <- arbitrary
          if k then f else g
  (pred, ad) <- if null as then f else h
  args <- replicateM (parity pred) (aArg ad)
  return (Pred (pid pred) $ args, ad)

aDefAnd size arbd = do
     (cl1, arbd1) <- aDefClause (size-1) arbd
     (cl2, arbd2) <- aDefClause (size-1) arbd1
     return (AndClause cl1 cl2, arbd2)

aConAnd size arbd = do
     (cl1, arbd1) <- aConClause (size-1) arbd
     (cl2, arbd2) <- aConClause (size-1) arbd1
     return (AndClause cl1 cl2, arbd2)

--aImply :: PredType -> ArbData -> Gen (LFPClause a)
aConImply size arbd = do
          (cl, arbd1) <- aConPred arbd
          (pre, arbd2) <- aPre (size-1) arbd1
          return (ConImply cl pre, arbd2)

aDefImply size arbd = do
       (cl, arbd1) <- aDefPred arbd
       (pre, arbd2) <- aPre (size-1) arbd1
       return (DefImply pre cl, arbd2)

--aForall :: PredType -> ArbData -> Gen (LFPClause a)
aDefForall size arbd = do
           let x = List.length . aVars $ arbd
           (cl, arbd1) <- aDefClause (size-1) (addVar x arbd)
           return (ForAll x cl, deleteVar arbd1)

aConForall size arbd = do
           let x = List.length . aVars $ arbd
           (cl, arbd1) <- aConClause (size-1) (addVar x arbd)
           return (ForAll x cl, deleteVar arbd1)