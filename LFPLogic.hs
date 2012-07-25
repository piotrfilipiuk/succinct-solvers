{-# Language GADTs,StandaloneDeriving,FlexibleInstances #-}
module LFPLogic where

import Test.QuickCheck
import Control.Monad
import Random

import qualified Data.IntSet as IntSet(union,toList)
import qualified Data.IntMap as IntMap(IntMap,insert,map,(!),keys,fromList,empty)
import qualified Data.Map as Map(empty,foldrWithKey,insert,fromList,fold)
import qualified Data.List as List(zip,and,map,foldr,length)
import Data ( Formula(..)
            , Argument(..)
            , Clause(..)
            , Pre(..)
            , negatePre
            , substituteNegQuery 
            , toPre
            , getVariableId
            , deleteVariable
            , addVariable
            , SolverData(..)
            , TranslData(..)
            , tryGetPredId
            , createArgs
            , maxFormulaDepth
            , maxArity
            , PredData(..)
            , PredType(..) 
            , isConPred
            , getPreds
            , substitute 
            , uniqueMemIds
            , depthPre
            , clauseDepth )

import ALFPLogic(replaceArgInPre,replaceArgInArgs)

import Control.Monad.State
import Data.Maybe

type SCC = [Int]

data LFPGraphMonad = State LFPGraphState

data LFPVertex = LFPVertex { vidx :: Maybe Int
                           , vll :: Maybe Int } 
                 deriving(Show)
                         
setVertex n = LFPVertex { vidx = Just n, vll = Just n }
isUndefined v = isNothing . vidx $ v
setLowLink n v = v { vll = Just n }

data LFPGraph = LFPGraph { vertices :: IntMap.IntMap LFPVertex
                         , succs :: IntMap.IntMap [Int] }
                deriving(Show)

gIsUndefined vid graph = isUndefined vrtx
                         where vs = vertices graph
                               vrtx = vs IntMap.! vid
gSetVertex n vid graph = graph { vertices = vs' }
                          where vs = vertices graph
                                vs' = IntMap.insert vid (setVertex n) vs

gSetLowLink :: Int -> Int -> LFPGraph -> LFPGraph
gSetLowLink n vid graph = graph { vertices = vs' }
                        where vs = vertices graph
                              vrtx = vs IntMap.! vid
                              vs' = IntMap.insert vid (setLowLink n vrtx) vs

data LFPGraphState = LFPGSt { graph :: LFPGraph
                            , stack :: [Int] 
                            , index :: Int 
                            , sccs :: [SCC] }
                     deriving(Show)

stIsUndefined vid st = gIsUndefined vid . graph $ st
stSetVertex vid st = st { graph = gSetVertex idx vid grph, index = idx + 1 }
                      where idx = index st
                            grph = graph st
stPush v st = let xs = stack st 
              in st { stack = v:xs }

stGetSuccs :: Int -> LFPGraphState -> [Int]
stGetSuccs vid st = let ss = succs . graph $ st
                  in ss IntMap.! vid

stSetLowLink :: Int -> Int -> LFPGraphState -> LFPGraphState
stSetLowLink n vid st = let graph' = gSetLowLink n vid . graph $ st
                      in st { graph = graph' }
stIsOnStack v st = v `elem` (stack st)
stGetLowLink vid st = fromJust. vll $ vrtx
                   where vrtx = vs IntMap.! vid
                         vs = vertices . graph $ st
stGetIndex vid st = fromJust . vidx $ vrtx
                   where vrtx = vs IntMap.! vid
                         vs = vertices . graph $ st
                         
createScc vertex st = st { stack = xs, sccs = (x:scc):ss }
                      where stck = stack st
                            (scc,x:xs) = break (vertex==) stck
                            ss = sccs st

isStratified' sd = case tryAsgnRanks sd of
                        Just ranks -> isStratified (cls sd) ranks
                        Nothing -> Prelude.False

isStratified cls ranks = List.and . List.map f $ xs
             where xs = List.zip [1..] $ cls
                   f (i, cl) = isStratClause (getClause cl) i ranks

isStratClause (Assert (Pred p _)) i ranks = i == (ranks IntMap.! p)
isStratClause (Constr (Pred p _)) i ranks = i == (ranks IntMap.! p)
isStratClause (ConImply (Pred p _) cond) i ranks = 
              (isStratCond cond i ranks) && i == (ranks IntMap.! p)
isStratClause (DefImply cond (Pred p _)) i ranks =
              (isStratCond cond i ranks) && i == (ranks IntMap.! p)
isStratClause (AndClause cl1 cl2) i ranks =
              isStratClause cl1 i ranks && isStratClause cl2 i ranks
isStratClause (ForAll _ cl) i ranks = isStratClause cl i ranks

isStratCond (Query p args) i ranks = i >= (ranks IntMap.! p)
isStratCond (NegQuery p args) i ranks = i > (ranks IntMap.! p)
isStratCond (AndPre pre1 pre2) i ranks =
            isStratCond pre1 i ranks && isStratCond pre2 i ranks
isStratCond (Or pre1 pre2 idx) i ranks =
            isStratCond pre1 i ranks && isStratCond pre2 i ranks
isStratCond (ForallPre x pre) i ranks = isStratCond pre i ranks
isStratCond (Exist x pre idx) i ranks = isStratCond pre i ranks
isStratCond _ _ _ = Prelude.True

{-
assignRanks :: SolverData [LFPLayer] -> IntMap.IntMap Int
assignRanks sd = fst . foldr f (zeroRanks, length xs) $ xs
            where xs = cls sd
                  f layer (pds, j) = (asgnRanks (getClause layer) j pds, j-1)
                  zeroRanks = Map.fold g IntMap.empty (predicates sd)
                  g pd acc = let id = pid pd
                             in IntMap.insert id 0 acc
-}
tryAsgnRanks sd = foldM f zeroRanks xs
             where xs = List.zip [1..] $ cls sd
                   f acc (i, layer) = asgnRanks (getClause layer) i acc
                   zeroRanks = Map.fold g IntMap.empty (predicates sd)
                   g pd acc = let id = pid pd
                              in IntMap.insert id 0 acc

asgnRanks (Assert (Pred p _)) j preds = asgnR p j preds
asgnRanks (Constr (Pred p _)) j preds = asgnR p j preds
asgnRanks (ConImply (Pred p _) cond) j preds = asgnR p j preds
asgnRanks (DefImply cond (Pred p _)) j preds = asgnR p j preds
asgnRanks (AndClause cl1 cl2) j preds = asgnRanks cl1 j preds 
                                        >>= asgnRanks cl2 j
asgnRanks (ForAll _ cl) j preds = asgnRanks cl j preds

asgnR pred j preds
      | rank == 0 = Just $ IntMap.insert pred j preds
      | rank == j = Just preds
      | otherwise = Nothing
      where rank = preds IntMap.! pred

createLFPGraph layer preds = LFPGraph { succs = successors, vertices = xs }
               where nullVertex = LFPVertex { vidx = Nothing, vll = Nothing }
                     xs = foldr (\p acc -> IntMap.insert p nullVertex acc) 
                                IntMap.empty 
                                preds
                     ss = getSuccsClause (getClause layer) IntMap.empty
                     successors = IntMap.map IntSet.toList ss
                     
getSuccsClause (ConImply (Pred p _) pre) succs =
                  IntMap.insert p xs succs
                  where xs = IntSet.union (succs IntMap.! p) (getPreds pre)
getSuccsClause (DefImply pre (Pred p xs)) succs =
                  IntMap.insert p xs succs
                  where xs = IntSet.union (succs IntMap.! p) (getPreds pre)
getSuccsClause (AndClause cl1 cl2) succs =
                  getSuccsClause cl2 . getSuccsClause cl1 $ succs
getSuccsClause (ForAll x cl) succs =
                  getSuccsClause cl succs
getSuccsClause _ succs = succs

computeScc grph = foldr doit initialState vs
           where initialState = LFPGSt { graph = grph
                                       , stack = []
                                       , index = 0
                                       , sccs = [] }
                 vs = IntMap.keys . vertices $ grph
                 doit vid st = if stIsUndefined vid st
                               then strongConnect vid st
                               else st

strongConnect vid st = 
            if stGetIndex vid st' == stGetLowLink vid st'
            then createScc vid st'
            else st'
            where ys = stGetSuccs vid st
                  st' = foldr (checkSucc vid) 
                              (stPush vid . stSetVertex vid $ st) 
                              ys

checkSucc vid wid st = 
          if stIsUndefined wid st
          then let st' = strongConnect wid st
                   minval = min (stGetLowLink vid st') (stGetLowLink wid st')
               in stSetLowLink minval vid st'
          else if stIsOnStack wid st
               then let minval = min (stGetLowLink vid st) (stGetIndex wid st)
                    in stSetLowLink minval vid st
               else st

--initialState = LFP
nullVertex = LFPVertex { vidx = Nothing, vll = Nothing }
vs = IntMap.fromList [(0, nullVertex), (1, nullVertex), (2, nullVertex)]
depts = IntMap.fromList [(0,[1]), (1,[0]), (2,[1])]
g = LFPGraph { vertices = vs, succs = depts }

ist = LFPGSt { graph = g
             , stack = []
             , index = 0
             , sccs = [] }

sumMe xs = execState (mapM f xs) 0
      where f x = do
              st <- get
              --put $ st+x
              g x st
            g x y = put $ x+y

--trajan :: LFPGraph -> LFPGraphMonad
trajan grph = execState eval initialState
  where initialState = LFPGSt { graph = grph
                              , stack = []
                              , index = 0
                              , sccs = [] }
        eval = do
          st <- get
          mapM mf . IntMap.keys . vertices . graph $ st
          return st

--mf :: Int -> LFPGraphMonad
mf vid = do 
  st <- get
  if stIsUndefined vid st 
  then scc vid
  else return st

--scc :: Int -> LFPGraphMonad
scc vid = do
  st <- get
  put . stSetVertex vid $ st
  put . stPush vid $ st
  let ys = stGetSuccs vid st
  mapM (fsucc vid) ys
  if stGetIndex vid st == stGetLowLink vid st
  then do
    put . createScc vid $ st
    return st
  else return st

--fsucc :: Int -> Int -> LFPGraphMonad
fsucc vid wid = do
  st <- get
  if stIsUndefined wid st
  then do
       scc wid
       let minval = min (stGetLowLink vid st) (stGetLowLink wid st)      
       put . stSetLowLink minval wid $ st
       return st
  else if stIsOnStack wid st
       then do
         let minval = min (stGetLowLink vid st) (stGetIndex wid st)      
         put . stSetLowLink minval wid $ st
         return st
  else return st

data Pred = Pred Int [Argument]
     deriving(Show)

type Definition = Pred
type Constraint = Pred

data FormulaLayer
     = DefFormula Formula
     | ConFormula Formula

getFormula (DefFormula f) = f
getFormula (ConFormula f) = f

data LFPLayer
     = DefLayer (LFPClause Definition)
     | ConLayer (LFPClause Constraint)
     deriving(Show)

isConLayer (DefLayer _) = Prelude.False
isConLayer (ConLayer _) = Prelude.True

isDefLayer (DefLayer _) = Prelude.True
isDefLayer (ConLayer _) = Prelude.False

getClause (DefLayer c) = c
getClause (ConLayer c) = c

type LFPCls = [LFPLayer]

data LFPClause a where
     Assert :: Definition -> LFPClause Definition
     Constr :: Constraint -> LFPClause Constraint
     ConImply :: Constraint -> Pre -> LFPClause Constraint
     DefImply :: Pre -> Definition -> LFPClause Definition
     AndClause :: LFPClause a -> LFPClause a -> LFPClause a
     ForAll :: Int -> LFPClause a -> LFPClause a

deriving instance (Show a) => Show (LFPClause a)

depthClause (AndClause cl1 cl2) = max (depthClause cl1) (depthClause cl2)
depthClause (ForAll _ cl) = 1 + depthClause cl
depthClause (ConImply _ pre) = depthPre pre
depthClause (DefImply pre _) = depthPre pre
depthClause _ = 0

replaceArgInDefClause (Assert pred) old new = (Assert pred')
  where pred' = replaceArgInPred pred old new
replaceArgInDefClause (AndClause cl1 cl2) old new = AndClause cl1' cl2'
  where cl1' = replaceArgInDefClause cl1 old new
        cl2' = replaceArgInDefClause cl2 old new
replaceArgInDefClause (DefImply pre pred) old new = DefImply pre' pred'
  where pre' = replaceArgInPre pre old new
        pred' = replaceArgInPred pred old new
replaceArgInDefClause (ForAll x cl) old new = ForAll x cl'
  where cl' = replaceArgInDefClause cl old new

replaceArgInConClause (Constr pred) old new = (Constr pred')
  where pred' = replaceArgInPred pred old new
replaceArgInConClause (AndClause cl1 cl2) old new = AndClause cl1' cl2'
  where cl1' = replaceArgInConClause cl1 old new
        cl2' = replaceArgInConClause cl2 old new
replaceArgInConClause (ConImply pred pre) old new = ConImply pred' pre'
  where pre' = replaceArgInPre pre old new
        pred' = replaceArgInPred pred old new
replaceArgInConClause (ForAll x cl) old new = ForAll x cl'
  where cl' = replaceArgInConClause cl old new

replaceArgInPred (Pred p args) old new = Pred p args'
                 where args' = replaceArgInArgs args old new

toALFPSolverData :: SolverData [LFPLayer] -> SolverData Clause
toALFPSolverData sd =
  sd { cls = cls', predicates = preds, maxClauseDepth = clauseDepth cls'}
  where --cls' = snd . uniqueMemIds 0 . foldr (\cl acc -> And acc cl) (toALFP . cls $ sd) $ defcls --xs
        --cls' = snd . uniqueMemIds 0 . translToALFP . cls $ sd
        cls' = snd . uniqueMemIds 0 . foldr (\cl acc -> And acc cl) (translToALFP . cls $ sd) $ defcls
        {-(preds, xs) = Map.foldrWithKey doit (predicates sd, []) (predicates sd)
        doit key val (tab, xs) = 
          if isConPred val 
          then (Map.insert (key++"_c") pd tab, forall:xs)
          else (tab, xs)
          where p = pid $ val
                pd = val { pid = complIdx p, ptype = DefPred }
                vs = [0..(parity val)-1]
                args = map (\x -> Var x) vs
                cl = Imply (NegQuery (complIdx p) args) (Assertion p args)
                forall = foldr (\x c -> Forall x c) cl vs -}
        preds = Map.foldrWithKey h (predicates sd) (predicates sd)
        h key val acc =
          if isConPred val
          then let pd = val { pid = complIdx . pid $ val, ptype = DefPred }
               in Map.insert (key++"_c") pd acc
          else acc
        fn pd acc = 
           if isConPred pd
           then cl : acc
           else acc  
           where len = parity pd
                 p = pid pd
                 args = List.map Var [0..len-1]
                 imply = Imply (NegQuery (complIdx p) args) (Assertion p args)
                 cl = List.foldr f imply [0..len-1]
                 f x acc = Forall x acc
        defcls = Map.fold fn [] (predicates sd)
{-        xs = foldl g [] . cls $ sd
                where g acc layer = 
                        if isConLayer layer 
                        then (newDefClause . getClause $ layer) : acc
                        else acc -}

translToALFP :: [LFPLayer] -> Clause
translToALFP cls = foldl1 (\acc cl -> And acc cl) xs
  where xs = map f cls
        f l = if isDefLayer l then cl1 else And cl1 cl2
          where cl1 = layerToALFP l
                ps = getConPidArtis l
                cl2 = foldl1 (\acc cl -> And acc cl) . map h $ ps
                h (p, len) = List.foldr g imply [0..len-1]
                             where args = List.map Var [0..len-1]
                                   imply = Imply (NegQuery (complIdx p) args) (Assertion p args)
                g x acc = Forall x acc

toALFP :: [LFPLayer] -> Clause
toALFP cls = foldl1 (\acc cl -> And acc cl) xs
             where xs = toALFP' cls

toALFP' :: [LFPLayer] -> [Clause]
toALFP' cls = map layerToALFP cls

layerToALFP :: LFPLayer -> Clause
layerToALFP layer = clauseToALFP . getClause $ layer
--layerToALFP (ConLayer con) = clauseToALFP con

clauseToALFP :: LFPClause t -> Clause
clauseToALFP (Assert (Pred p xs)) = Assertion p xs
clauseToALFP (Constr (Pred p xs)) = Assertion (complIdx p) xs
clauseToALFP (ConImply (Pred p xs) cond) = imply
             where cond' = substituteNegQuery p (complIdx p) . negatePre $ cond
                   {-cond' =  AndPre (substitute p TruePre cond)
                                  (substituteNegQuery p (complIdx p) . negatePre $ cond) -}
                   imply = Imply cond' $ Assertion (complIdx p) xs
                   
clauseToALFP (DefImply pre (Pred p xs)) = Imply pre $ Assertion p xs
clauseToALFP (AndClause c1 c2) = And (clauseToALFP c1) (clauseToALFP c2)
clauseToALFP (ForAll x c) = Forall x $ clauseToALFP c

--newDefLayer (ConLayer con) = newDefClasue con
{-
newDefClause :: LFPClause t -> Clause
newDefClause (ConImply (Pred p xs) cond) = cl
             where cond' = AndPre (substitute p TruePre cond) (NegQuery (complIdx p) xs)
                   cl = Imply cond' (Assertion p xs)
newDefClause (AndClause c1 c2) = And (newDefClause c1) (newDefClause c2)
newDefClause (ForAll x c) = Forall x . newDefClause $ c
newDefClause (Constr (Pred p xs)) = cl --Imply (NegQuery (complIdx p) xs) (Assertion p xs)
             where len = List.length xs
                   args = List.map Var [0..len-1]
                   imply = Imply (NegQuery (complIdx p) args) (Assertion p args)
                   cl = List.foldr f imply [0..len-1]
                   f x acc = Forall x acc
-}
complIdx p = -p - 1

getConPidArtis :: LFPLayer -> [(Int, Int)]
getConPidArtis (ConLayer l) = getConPidArtisClause l []

getConPidArtisClause :: LFPClause Constraint -> [(Int, Int)] -> [(Int, Int)]
getConPidArtisClause (Constr (Pred p xs)) acc = (p, List.length xs) : acc
getConPidArtisClause (ConImply (Pred p xs) _) acc = (p, List.length xs) : acc
getConPidArtisClause (AndClause c1 c2) acc = 
  getConPidArtisClause c2 . getConPidArtisClause c1 $ acc
getConPidArtisClause (ForAll _ c) acc = getConPidArtisClause c acc

toLFP :: [FormulaLayer] -> SolverData [LFPLayer]
toLFP xs = result
  where (cl, td) = foldr doit ([], itd) xs
        doit f (cls, td) = let (cl, td') = toLFPLayer f td 
                           in (cl:cls, td')
        result = SolverData { cls = cl
                            , predicates = preds td
                            , atoms = atms td
                            , maxPredicateArity = mpa
                            , maxClauseDepth = mcd }
        mpa = maximum . map (\f -> maxArity (getFormula f) 0 id) $ xs
        mcd = maximum . map (\f -> maxFormulaDepth (getFormula f) 0 0 (\ x y -> y)) $ xs
        itd = TranslData { vars = Map.empty
                         , preds = Map.empty
                         , atms = Map.empty
                         , nextMemIdx = 0 }

--toLFPLayer :: FormulaLayer -> TranslData a -> (LFPLayer, TranslData a)
toLFPLayer (DefFormula f) td = let (cl, td') = toLFPDefClause f (\a b -> (a,b)) td
                               in (DefLayer cl, td')    
toLFPLayer (ConFormula f) td = (ConLayer cl, td')
                               where (cl, td') = toLFPConClause f (\a b -> (a,b)) td

{-toLFPDefClause
  :: Formula
     -> (LFPClause Definition -> TranslData a -> (LFPClause Definition, TranslData a))
     -> TranslData a
     -> (LFPClause Definition, TranslData a)-}
toLFPDefClause (Predicate p args) cont td = cont (Assert $ Pred pId newArgs) td'
         where (pId, newPs) = tryGetPredId p (Just DefPred) (length args) (preds td)
               (newArgs, newAtoms) = createArgs args (vars td) (atms td)
               td' = td { preds = newPs, atms = newAtoms }

toLFPDefClause (Rightarrow pre def) cont td = 
         toPre pre (\p -> toPred def (Just DefPred) (\c -> cont (DefImply p c))) td

toLFPDefClause (Wedge cl1 cl2) cont td =
         toLFPDefClause cl1 (\c1 -> toLFPDefClause cl2 (\c2 -> cont (AndClause c1 c2))) td

toLFPDefClause (A s cl) cont td =
         toLFPDefClause cl cont' td'
         where cont' ccl ctd = cont (ForAll (getVariableId s td') ccl) (deleteVariable s ctd)
               td' = addVariable s td

{-toLFPConClause
  :: Formula
     -> (LFPClause Constraint -> TranslData a -> (LFPClause Constraint, TranslData a))
     -> TranslData a
     -> (LFPClause Constraint, TranslData a)-}
toLFPConClause (Predicate p args) cont td = cont (Constr $ Pred pId newArgs) td'
         where (pId, newPs) = tryGetPredId p (Just ConPred) (length args) (preds td)
               (newArgs, newAtoms) = createArgs args (vars td) (atms td)
               td' = td { preds = newPs, atms = newAtoms }

toLFPConClause (Rightarrow con pre) cont td = 
         toPred con (Just ConPred) (\p -> toPre pre (\c -> cont (ConImply p c))) td

toLFPConClause (Wedge cl1 cl2) cont td = 
         toLFPConClause cl1 (\c1 -> toLFPConClause cl2 (\c2 -> cont (AndClause c1 c2))) td

toLFPConClause (A s cl) cont td = 
         toLFPConClause cl cont' td'
         where cont' ccl ctd = cont (ForAll (getVariableId s td') ccl) (deleteVariable s ctd)
               td' = addVariable s td
toLFPConClause c _ _ = error $ "Syntax error: " ++ (show c)
  
--toPred :: Formula -> (Pred -> TranslData b -> (LFPClause a, TranslData b)) -> TranslData b -> (LFPClause a, TranslData b)
toPred (Predicate p args) pty cont td = cont (Pred pId newArgs) td'
         where (pId, newPs) = tryGetPredId p pty (length args) (preds td)
               (newArgs, newAtoms) = createArgs args (vars td) (atms td)
               td' = td { preds = newPs, atms = newAtoms }


--newtype CTLSolverData = SolverData [FormulaLayer]
{-
instance Arbitrary (SolverData [LFPLayer]) where
         arbitrary = do
           bitsCount <- choose (1,5::Int)
           let pidT = 0
               pidPhi = 1
               pidProp = 2
               statesCount = 2^bitsCount
               states = [0..statesCount-1]
               mf s = do
                 succCount <- choose (1,10)
                 replicateM succCount ma
                 where ma = do
                         s' <- choose (0,statesCount-1)
                         let args = [Const s, Const s']
                         return . Assert . Pred pidT $ args
           ts <- liftM concat . mapM mf $ states
           phiStatesCount <- choose (0,statesCount-1)
           let mphi = do
                 s <- choose (0,statesCount-1)
                 return . Assert . Pred pidPhi $ [Const s]
           phis <- replicateM phiStatesCount mphi
           let defLayer = DefLayer . foldr1 (\c acc -> AndClause acc c) $ phis++ts
               propLayer = ConLayer $ AndClause cl1 cl2
                   where cl1 = ForAll s $ ConImply con cond1
                         cl2 = ForAll s $ ConImply con cond2
                         con = Pred pidProp [Var s]
                         cond1 = Query pidPhi [Var s]
                         cond2 = ForallPre s' $ Or (NegQuery pidT [Var s, Var s']) 
                                                   (Query pidProp [Var s']) memIdx
                         memIdx = 0
                         s = 0
                         s' = 1
               preds = [("T", predT),("phi", predPhi),("R", predProp)]
               predPhi = PredData { pid = pidPhi , parity = 1 , ptype = DefPred }
               predT = PredData { pid = pidT , parity = 2 , ptype = DefPred }
               predProp = PredData { pid = pidProp , parity = 1 , ptype = ConPred }
               sd = SolverData { cls = [defLayer,propLayer]
                               , predicates = Map.fromList preds
                               , atoms = Map.fromList . map (\x -> (show x, x)) $ states
                               , maxPredicateArity = 2
                               , maxClauseDepth = 2 }
           return sd
-}