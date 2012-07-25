module Magic(doMagic) where

import Data.Maybe(fromJust)
import Data.IntSet(IntSet)
import qualified Data.IntSet as IntSet
import Data.Set(Set)
import qualified Data.Set as Set
import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.List as List

import Data
import Parser
import Lexer

import BddIO

testclause1 = "R(a) & (A x. R(x) => S(x))"
testquery1 = "S(a)"

testclause2 = "R(a,b) & (A x. A y. R(x,y) => T(x,y)) & (A x. A y. (E z. R(x,z) & T(z,y)) => T(x,y))"
testquery2 = "T(a,_)"

testclause3 = "T(a,b) & R(a) & (A x. R(x) => S(x)) & (A x. A y. T(x,y) & S(x) => S(y))"
testquery3 = "S(a)"

testclause4 = "T(a,b) & R(a) & (A x. R(x) => S(x)) & (A x. A y. S(x) & T(x,y) => S(y))"
testquery4 = "S(a)"

printClauses sd = mapM (\c -> print . showClause c ps $ atms) . cls $ sd where
  ps = Map.foldrWithKey (\k v acc -> IntMap.insert v k acc) IntMap.empty 
       . Map.map (\pd -> pid pd) 
       . predicates $ sd
  atms = Map.foldrWithKey (\k v acc -> IntMap.insert v k acc) IntMap.empty 
         . atoms $ sd

test str query = (print . predicates $ sd') >> printClauses sd' where
  sdss = toSolverData . formula . alexScanTokens $ str
  sd = SolverData { cls = toClauseList $ cls sdss
                  , predicates = predicates sdss
                  , atoms = atoms sdss
                  , maxPredicateArity = maxPredicateArity sdss
                  , maxClauseDepth = maxClauseDepth sdss }  
  (Right (p,xs)) = getQuery query
  predId = pid $ (predicates sd) Map.! p
  args = List.map f xs
  f s = if s == "_" then Var 0 else Const $ (atoms sd) Map.! s
  sd' = doMagic sd (predId, args)

myCl = Forall 0 (Imply myPre (Assertion 3 [Var 0]))
myPre = Exist 1 (AndPre (Query 2 [Var 0, Var 1]) (Query 3 [Var 1])) 0
myAnP = AnP { anPId = 3, pannot = [Bound] }
derived = IntSet.singleton 3
pInfo = PI { toIdx = Map.empty
           , allPreds = IntSet.fromList [2,3]
           , nextIdx = 2
           , allPredsDta = Set.empty }
wl = WL { todo = Set.singleton myAnP
        , done = Set.empty }

--x = magicClause myCl myAnP derived IntSet.empty pInfo
--a = mkCl myCl myAnP derived pInfo

r = process wl derived pInfo (asrtIn [myCl]) IntMap.empty

--doMagic :: SolverData [Clause] -> SolverData [Clause]
doMagic sd (p,xs) = 
  sd { cls = newClauses, predicates = newPredicates }
  where (base, derived) = dvdPreds sd
        mapping = asrtIn . cls $ sd
        factCls = facts . cls $ sd
        ranks = IntMap.assocs . snd . asgnRanksClauses ns . cls $ sd
        sortedRanks = List.sortBy (\(_,r1) (_,r2) -> compare r1 r2) ranks
        anP = AnP { anPId = p, pannot = bs }
        bs = List.map argToAnnot xs
        ps = List.map (\pd -> pid pd) . Map.elems . predicates $ sd
        magicPD = PredData { pid = 1 + List.maximum ps
                           , parity = List.length bs
                           , ptype = DefPred }
        ns = List.map (\pd -> pid pd) 
             . Map.elems 
             . Map.insert ("magic" ++ show p) magicPD 
             . predicates $ sd
        derivedSet = IntSet.fromList derived
        pInfo = PI { toIdx = Map.singleton anP (pid magicPD)
           , allPreds = IntSet.fromList ns
           , nextIdx = 1 + List.maximum ns
           , allPredsDta = Set.singleton magicPD }
        wl = WL { todo = Set.singleton anP
                , done = Set.empty }
        (pInfo', res) = case IntMap.lookup p mapping of
                Just cls -> process wl derivedSet pInfo mapping IntMap.empty 
                Nothing  -> (pInfo, IntMap.empty)
        f acc (p,_) = if IntSet.member p derivedSet 
                      then (acc ++ mags ++ mods)
                      else acc
                      where (mags,mods) = 
                              case IntMap.lookup p res of 
                                Just (lst1,lst2) -> (lst1,lst2)
                                Nothing -> ([],[])  
        newClauses = List.foldl f (seed:factCls) ranks
        newPredicates = Set.fold g (predicates sd) . allPredsDta $ pInfo'
        g elem acc = Map.insert name elem acc
                     where name = "magic" ++ (show . pid $ elem)
        magicSeed = (toIdx pInfo') Map.! anP
        seedArgs = List.filter isConst xs
        seed = Assertion magicSeed seedArgs

process wl derived pInfo asrtin res
  | isEmpty wl = (pInfo, res)
  | otherwise = process wl' derived pInfo' asrtin res'
  where (anP, wl') = getElem wl
        p = anPId anP
        (pInfo', magicCls, modifiedCls, wl'') = case IntMap.lookup p asrtin of
                Just cs -> rewriteClauses cs anP derived pInfo wl'
                Nothing  -> (pInfo, [], [], wl')
        --(magicCls, modifiedCls) = List.foldl (\(as,bs) (xs,y) -> (as ++ xs, y : bs)) ([],[]) cls
        res' = case IntMap.lookup p res of
                 Just (ms, cs) -> IntMap.insert p (magicCls ++ ms, modifiedCls ++ ms) res
                 Nothing -> IntMap.insert p (magicCls, modifiedCls) res

--rewriteClauses :: [Clause] -> AnP -> IntSet -> PInfo -> [([Clause], Clause)]
rewriteClauses cls anP derived pInfo wl = List.foldl f (pInfo,[],[],wl) cls
  where f (pi,as,bs,wl) cl = let (pi',ss,b,wl') = rewriteClause cl anP derived pi wl
                          in (pi',as ++ ss,b:bs,wl')
--  List.map (\cl -> rewriteClause cl anP derived pInfo) cls

--rewriteClause :: Clause -> AnP -> IntSet -> PInfo -> ([Clause], Clause)
rewriteClause cl anP derived pInfo wl = 
  (pInfo', magicCls, cl', wl')
  where (pInfo', magicCls, wl') = mkCl cl anP derived pInfo wl
        cl' = insertMagic cl anP pInfo'

data WorkList = WL { done :: Set AnP
                   , todo :: Set AnP }

insert anP wl = if Set.member anP . done $ wl 
                then wl 
                else wl { todo = Set.insert anP . todo $ wl }

isEmpty wl = Set.null . todo $ wl

--Returns an element from the todo set, and a modified worklist.
--Precondition: the todo set cannot be empty.
getElem wl = (x, wl')
          where x = List.head . Set.elems . todo $ wl
                wl' = WL { done = Set.insert x . done $ wl
                         , todo = Set.delete x . todo $ wl }

mkCl (Assertion p xs) anP derived pInfo wl = (pInfo, [], wl)
mkCl (Forall x cl) anP derived pInfo wl = mkCl cl anP derived pInfo wl
mkCl (Imply pre (Assertion p xs)) anP derived pInfo wl =
  mk pre [query] derived IntSet.empty pInfo' [] wl cont
  where (magicP, ms, pInfo') = mkMagicP anP pInfo xs
        query = Query magicP ms
        cont qs drvd vars pInf acc wl = (pInf, acc, wl)
mkCl (And cl1 cl2) anP derived pInfo wl = error "mkCl (And cl1 cl2) vars."

mk q@(Query p args) qs derived vars pInfo acc wl cont =
  if IntSet.member p derived
  then if (not . List.null $ xs) 
       then cont [Query magicP xs] derived vars' pInfo' (magicClause : acc) wl'
       else cont (Query magicP xs:qs) derived vars' pInfo' acc wl'
  else cont (q:qs) derived vars' pInfo acc wl
  where anP = mkAnP p vars args
        wl' = insert anP wl
        (magicP, xs, pInfo') = mkMagicP anP pInfo args
        magicPre = List.foldl1 (\acc qry -> AndPre acc qry) qs
        imp = (Imply magicPre (Assertion magicP xs))
        vars' = (IntSet.union vars . getVars $ args)
        magicClause = List.foldl (\cl x -> Forall x cl) imp . IntSet.toList $ vars'
mk nq@(NegQuery _ args) qs derived vars pInfo acc wl cont =
  cont (nq:qs) derived (IntSet.union vars . getVars $ args) pInfo acc wl
mk (AndPre pre1 pre2) qs derived vars pInfo acc wl cont =
  let cont' qs' derived' vars' pInfo' acc' wl' = mk pre2 qs' derived' vars' pInfo' acc' wl' cont
  in mk pre1 qs derived vars pInfo acc wl cont'
mk (ForallPre x pre) qs derived vars pInfo acc wl cont =
  mk pre qs derived vars pInfo acc wl cont
mk (Exist x pre n) qs derived vars pInfo acc wl cont =
  mk pre qs derived vars pInfo acc wl cont
mk Data.TruePre qs derived vars pInfo acc wl cont =
  cont qs derived vars pInfo acc wl
mk Data.FalsePre qs derived vars pInfo acc wl cont =
  cont qs derived vars pInfo acc wl

insertMagic (Forall x cl) anP pInfo = 
  Forall x $ insertMagic cl anP pInfo
insertMagic (Imply pre (Assertion p xs)) anP pInfo = 
  Imply (AndPre query pre) (Assertion p xs)
  where (magicP, ms, pInfo') = mkMagicP anP pInfo xs
        query = Query magicP ms
insertMagic (And cl1 cl2) anP pInfo = error "insertMagic (And cl1 cl2) anP pInfo"
insertMagic (Assertion p xs) anP pInfo = (Assertion p xs) --error "insertMagic (Assertion p xs) anP pInfo"
insertMagic TT anP pInfo = error "insertMagic TT anP pInfo"

getVars args = List.foldl f IntSet.empty args
               where f acc (Var x) = IntSet.insert x acc
                     f acc _       = acc

data PInfo = PI { toIdx :: Map AnP Int
                , allPreds :: IntSet
                , nextIdx :: Int
                , allPredsDta :: Set PredData }

mkMagicP :: AnP -> PInfo -> [Argument] -> (Int, [Argument], PInfo)
mkMagicP anP pInfo args = (p, xs, pInfo')
           where (p, pInfo') = getMagicIdx anP pInfo
                 zs = List.zip args (pannot anP)
                 xs = List.foldl f [] zs
                 f acc (x, Bound) = x:acc
                 f acc (x, Free) = acc

getMagicIdx anP pInfo = 
  case Map.lookup anP . toIdx $ pInfo of
    Just n -> (n, pInfo)
    Nothing -> (idx, pInfo')
  where idx = until (\a -> not . IntSet.member a . allPreds $ pInfo) 
                    (\a -> a + 1) . nextIdx $ pInfo
        toIdx' = Map.insert anP idx . toIdx $ pInfo
        allPreds' = IntSet.insert idx . allPreds $ pInfo
        arity = List.length . List.filter isBound . pannot $ anP
        pd = PredData { pid = idx, parity = arity, ptype = DefPred }
        allPredsDta' = Set.insert pd . allPredsDta $ pInfo
        pInfo' = pInfo { toIdx = toIdx'
                       , allPreds = allPreds'
                       , nextIdx = idx + 1
                       , allPredsDta = allPredsDta' }

-- magicClause (Forall x cl) anP derived vars pInfo =
--   List.map (\cl -> Forall x cl) $ magicClause cl anP derived (IntSet.insert x vars) pInfo
-- magicClause (Imply pre (Assertion p xs)) anP derived vars pInfo =
--   mkMagicCl zipper derived vars pInfo [] cont
--   where (magicP, ms, pInfo') = mkMagicP anP pInfo xs
--         magicQ = Query magicP ms
--         zipper = (magicQ, [RightCrumb pre])
--         cont _ _ _ _ acc = acc

-- mkMagicCl z@(Query p args, bs) derived vars pInfo acc cont = 
--   if IntSet.member p derived 
--   then cont z derived vars pInfo (magicClause : acc)
--   else cont z derived vars pInfo (magicClause : acc) -- acc
--   where anP = mkAnP p vars args
--         (magicP, xs, pInfo') = mkMagicP anP pInfo args
--         (AndPre pre1 pre2, bs') = backtrack z
--         (magicPre, []) = topMost (pre1, bs')
--         magicClause = Imply magicPre (Assertion magicP xs)
-- mkMagicCl z@(NegQuery p args, bs) derived vars pInfo acc cont = 
--   cont z derived vars pInfo acc
-- mkMagicCl (AndPre pre1 pre2, bs) derived vars pInfo acc cont = 
--   let cont' _ derived' vars' pInfo' acc' = mkMagicCl (pre2, RightCrumb pre1:bs) 
--                                                 derived' vars' pInfo' acc' cont
--   in mkMagicCl (pre1, LeftCrumb pre2:bs) derived vars pInfo acc cont'
-- mkMagicCl (ForallPre x pre, bs) derived vars pInfo acc cont =
--   mkMagicCl (pre, ForallCrumb x:bs) derived vars' pInfo acc cont
--   where vars' = IntSet.insert x vars
-- mkMagicCl (Exist x pre i, bs) derived vars pInfo acc cont = 
--   mkMagicCl (pre, ExistCrumb x i:bs) derived vars' pInfo acc cont
--   where vars' = IntSet.insert x vars

-- backtrack z@(pre, bs) = fromJust $ if lastLeft bs then goUpUntilRight z else goUp z

--Divides predicates into 2 disjoint lists, (base predicates, derived predicates)
dvdPreds :: SolverData [Clause] -> ([Int], [Int])
dvdPreds sd = (IntSet.toList base, IntSet.toList derived)
  where all = IntSet.fromList . List.map Data.pid . Map.elems . predicates $ sd
        derived = List.foldl (\acc cl -> dvdPredsClause cl Prelude.False acc)
                  IntSet.empty (cls sd)
        base = IntSet.difference all derived

--If flag is True then we are on the right of implication
--Otherwise the are no implication in the clause.
dvdPredsClause :: Clause -> Bool -> IntSet -> IntSet
dvdPredsClause (Assertion p _) add derived =
  if add 
  then IntSet.insert p derived
  else derived

dvdPredsClause (Imply _ cl) _ derived =
  dvdPredsClause cl Prelude.True derived

dvdPredsClause (And cl1 cl2) add derived =
  dvdPredsClause cl2 add . dvdPredsClause cl1 add $ derived
  
dvdPredsClause (Forall _ cl) add derived =
  dvdPredsClause cl add derived

dvdPredsClause Data.TT add derived = derived

--For a given list of clauses, returns a mapping, that for each
--predicate returns a list of clauses asserting it.
asrtIn :: [Clause] -> IntMap.IntMap [Clause]
asrtIn cls = List.foldl f IntMap.empty cls
             where f acc cl = case asrtPred cl Prelude.False of
                                Just p -> update p cl acc
                                Nothing -> acc
                   update p cl tab = case IntMap.lookup p tab of
                                       Just cls -> IntMap.insert p (cl : cls) tab
                                       Nothing -> IntMap.insert p [cl] tab
{-
asrtPred :: Clause -> Maybe Int
asrtPred (Assertion p _) = Just p
asrtPred (Imply _ cl) = asrtPred cl
asrtPred (And cl1 cl2) = error "asrtPred (And cl1 cl2) _ should not happen."
asrtPred (Forall _ cl) = asrtPred cl
asrtPred Data.TT = Nothing
-}
--Returns an index of a derived predicate that is asserted in the clause.
--Precondition: the clause is transformed st. there is no conjunction of clauses.
asrtPred :: Clause -> Bool -> Maybe Int
asrtPred (Assertion p _) add = if add then Just p else Nothing
asrtPred (Imply _ cl) _ = asrtPred cl Prelude.True
asrtPred (And cl1 cl2) add = error "asrtPred (And cl1 cl2) _ should not happen."
asrtPred (Forall _ cl) add = asrtPred cl add
asrtPred Data.TT _ = Nothing

data Annot = Bound | Free deriving (Eq, Ord)

isBound Bound = Prelude.True
isBound Free = Prelude.False

argToAnnot (Const _) = Bound
argToAnnot (Var x)   = Free
                               
data AnP = AnP { anPId :: Int, pannot :: [Annot] } deriving (Eq, Ord)

mkAnP p vars args = AnP { anPId = p, pannot = boundArgs vars args }

boundArgs vars args = List.map (boundArg vars) args

boundArg _ (Const _) = Bound
boundArg vars (Var x) = if IntSet.member x vars then Bound else Free

facts cls = List.filter isFact cls

isFact (Assertion _ _) = Prelude.True
isFact (Imply _ _) = Prelude.False
isFact (And cl1 cl2) = isFact cl1 && isFact cl2
isFact (Forall _ cl) = isFact cl  
isFact TT = Prelude.True

asgnRanksClauses preds cls = List.foldl f (0, initialRanks) cls
  where initialRanks = List.foldl (\acc n -> IntMap.insert n 0 acc) IntMap.empty preds
        f (n,acc) cl = asgnRanksClause cl n acc

asgnRanksClause :: Clause -> Int -> IntMap Int -> (Int, IntMap Int)
asgnRanksClause (Assertion p _) curr acc = (curr, IntMap.insert p new acc)
  where new = max curr (acc IntMap.! p)
asgnRanksClause (Imply pre cl) curr acc = asgnRanksClause cl curr' acc'
  where (curr', acc') = asgnRanksPre pre curr acc
asgnRanksClause (And cl1 cl2) curr acc = asgnRanksClause cl2 curr' acc'
  where (curr', acc') = asgnRanksClause cl1 curr acc
asgnRanksClause (Forall _ cl) curr acc = asgnRanksClause cl curr acc
asgnRanksClause Data.TT curr acc = (curr, acc)

asgnRanksPre :: Pre -> Int -> IntMap Int -> (Int, IntMap Int)
asgnRanksPre (Query p _) curr acc = (curr, acc)
asgnRanksPre (NegQuery p _) curr acc = (curr+1, acc)
asgnRanksPre (AndPre pre1 pre2) curr acc = asgnRanksPre pre2 curr' acc'
  where (curr', acc') = asgnRanksPre pre1 curr acc
asgnRanksPre (Or pre1 pre2 _) curr acc = asgnRanksPre pre2 curr' acc'
  where (curr', acc') = asgnRanksPre pre1 curr acc
asgnRanksPre (Exist _ pre _) curr acc = asgnRanksPre pre curr acc
asgnRanksPre (ForallPre _ pre) curr acc = asgnRanksPre pre curr acc
asgnRanksPre _ curr acc = (curr, acc)
-- data Crumb
--           = LeftCrumb Pre
--           | RightCrumb Pre
--           | ForallCrumb Int 
--           | ExistCrumb Int Int 
--           deriving (Show)

-- type Crumbs = [Crumb]

-- lastLeft (LeftCrumb _:bs) = Prelude.True
-- lastLeft _                = Prelude.False

-- type Zipper = (Pre, Crumbs) 
 
-- goUp :: Zipper -> Maybe Zipper
-- goUp (pre, LeftCrumb r:bs) = Just (AndPre pre r, bs)
-- goUp (pre, RightCrumb l:bs) = Just (AndPre l pre, bs)
-- goUp (pre, ForallCrumb x:bs) = Just (ForallPre x pre, bs)
-- goUp (pre, ExistCrumb x i:bs) = Just (Exist x pre i, bs)
-- goUp (_, []) = Nothing

-- goUpUntilRight :: Zipper -> Maybe Zipper
-- goUpUntilRight (pre, RightCrumb l:bs) = Just (AndPre l pre, bs)
-- goUpUntilRight (pre, crumb) = goUp (pre, crumb) >>= goUpUntilRight

-- topMost :: Zipper -> Zipper
-- topMost (t,[]) = (t,[])
-- topMost z = case goUp z of
--               Just z' -> topMost z'
--               Nothing -> error "topMost z."