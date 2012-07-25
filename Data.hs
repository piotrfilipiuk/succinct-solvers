{-# LANGUAGE BangPatterns #-}
module Data where

import Data.Maybe(fromJust,isJust,isNothing)
import Data.List
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet(singleton,union,empty)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.DeepSeq
import Text.PrettyPrint.HughesPJ
import Data.Char

data Env = Env !(IntMap.IntMap (Maybe Int))

data Formula =
     Predicate String [String]
     | NegPredicate String [String]
     | Wedge Formula Formula
     | Vee Formula Formula
     | Rightarrow Formula Formula
     | A String Formula
     | E String Formula
     | True
     | False 
     | Equal String String
     | Nequal String String
     deriving (Show, Read)

data Pre =
     Query Int [Argument]
     | NegQuery Int [Argument]
     | AndPre Pre Pre
     | Or Pre Pre Int
     | ForallPre Int Pre
     | Exist Int Pre Int
     | Eq Argument Argument
     | Neq Argument Argument
     | TruePre
     | FalsePre
     deriving (Show, Read)

data Clause =
     Assertion Int [Argument]
     | And Clause Clause
     | Imply Pre Clause
     | Forall Int Clause
     | TT
     deriving (Show, Read)

getPreds (Query p _) = IntSet.singleton p
getPreds (NegQuery p _) = IntSet.singleton (-p)
getPreds (AndPre pre1 pre2) = IntSet.union (getPreds pre1) (getPreds pre2)
getPreds (Or pre1 pre2 idx) = IntSet.union (getPreds pre1) (getPreds pre2)
getPreds (ForallPre x pre) = getPreds pre
getPreds (Exist x pre idx) = getPreds pre
getPreds _ = IntSet.empty

negatePre (Query p args) = NegQuery p args
negatePre (NegQuery p args) = Query p args
negatePre (AndPre pre1 pre2) = Or (negatePre pre1) (negatePre pre2) 0
negatePre (Or pre1 pre2 idx) = AndPre (negatePre pre1) (negatePre pre2)
negatePre (ForallPre x pre) = Exist x (negatePre pre) 0
negatePre (Exist x pre idx) = ForallPre x $ negatePre pre
negatePre (Eq a1 a2) = Neq a1 a2
negatePre (Neq a1 a2) = Eq a1 a2
negatePre TruePre = FalsePre
negatePre FalsePre = TruePre


showArgs xs atms = concat . intersperse comma' . map (showArg atms) $ xs

showArg _ (Var x) = "x" ++ (show x)
showArg atms (Const n) = atms IntMap.! n

showPre (Query p args) prds atms =  p' ++ lparen' ++ args' ++ rparen'
        where p' = prds IntMap.! p
              args' = showArgs args atms
showPre (NegQuery p args) prds atms = not' ++ p' ++ lparen' ++ args' ++ rparen'
        where p' = prds IntMap.! p
              args' = showArgs args atms
showPre (AndPre pre1 pre2) prds atms = lparen' ++ 
                                        (showPre pre1 prds atms) ++ 
                                        and' ++ 
                                        (showPre pre2 prds atms) ++ 
                                        rparen'
showPre (Or pre1 pre2 _) prds atms = lparen' ++ 
                                      (showPre pre1 prds atms) ++ 
                                      or' ++ 
                                      (showPre pre2 prds atms) ++ 
                                      rparen'
showPre (ForallPre x pre) prds atms = lparen' ++ 
                                       forall' ++ "x" ++ (show x) ++ dot ++
                                       (showPre pre prds atms) ++ 
                                       rparen'
showPre (Exist x pre _) prds atms = lparen' ++ 
                                     exists' ++ "x" ++ (show x) ++ dot ++
                                     (showPre pre prds atms) ++ 
                                     rparen'
showPre (Eq a1 a2) prds atms = lparen' ++ a1' ++ eq ++ a2' ++ rparen'
        where a1' = showArg atms a1
              a2' = showArg atms a2
showPre (Neq a1 a2) prds atms = lparen' ++ a1' ++ neq ++ a2' ++ rparen'
        where a1' = showArg atms a1
              a2' = showArg atms a2
showPre TruePre prds atms = "true"
showPre FalsePre prds atms = "false"

showClause (Assertion p args) prds atms = p' ++ lparen' ++ args' ++ rparen'
           where p' = prds IntMap.! p
                 args' = showArgs args atms
showClause (And cl1 cl2) prds atms = lparen' ++ 
                                      (showClause cl1 prds atms) ++ 
                                      and' ++ newLine' ++
                                      (showClause cl2 prds atms) ++ 
                                      rparen'
showClause (Imply pre cl) prds atms = lparen' ++ 
                                       (showPre pre prds atms) ++ 
                                       imply ++ 
                                       (showClause cl prds atms) ++ 
                                       rparen'
showClause (Forall x cl) prds atms = lparen' ++ 
                                      forall' ++ "x" ++ (show x) ++ dot ++
                                      (showClause cl prds atms) ++ 
                                      rparen'
showClause TT prds atms = "TT"

eq = "="
neq = "!="
exists' = "E "
forall' = "A "
not' = "!"
and' = " & "
imply = " => "
comma' = ","
or' = " | "
lparen' = "("
rparen' = ")"
newLine' = "\n"
dot = ". "

substitute r pre' query@(Query p _) = 
           if r == p then pre' else query
--substitute _ _ negQuery@(NegQuery _ _) = negQuery
substitute r pre' (AndPre pre1 pre2) = 
           AndPre (substitute r pre' pre1) (substitute r pre' pre2)
substitute r pre' (Or pre1 pre2 idx) = 
           Or (substitute r pre' pre1) (substitute r pre' pre2) idx
substitute r pre' (ForallPre x pre) =
           ForallPre x (substitute r pre' pre)
substitute r pre' (Exist x pre idx) =
           Exist x (substitute r pre' pre) idx
substitute _ _ pre = pre

substituteNegQuery _ _ (Query p args) = Query p args
substituteNegQuery q s (NegQuery p args)
                   | q == p = Query s args
                   | otherwise = NegQuery p args
substituteNegQuery q s (AndPre pre1 pre2) = 
                   AndPre (substituteNegQuery q s pre1) (substituteNegQuery q s pre2)
substituteNegQuery q s (Or pre1 pre2 idx) = 
                   Or (substituteNegQuery q s pre1) (substituteNegQuery q s pre2) idx
substituteNegQuery q s (ForallPre x pre) = 
                   ForallPre x (substituteNegQuery q s pre)
substituteNegQuery q s (Exist x pre idx) = 
                   Exist x (substituteNegQuery q s pre) idx
substituteNegQuery _ _ pre = pre

uniqueMemIds memIdx (Imply pre cl) = (memIdx2, Imply pre' cl')
  where (memIdx1, pre') = uniqMemIds memIdx pre
        (memIdx2, cl') = uniqueMemIds memIdx1 cl
uniqueMemIds memIdx (And cl1 cl2) = (memIdx2, And cl1' cl2')
  where (memIdx1, cl1') = uniqueMemIds memIdx cl1
        (memIdx2, cl2') = uniqueMemIds memIdx1 cl2
uniqueMemIds memIdx (Forall x cl) = (memIdx', Forall x cl')
  where (memIdx', cl') = uniqueMemIds memIdx cl
uniqueMemIds memIdx cl = (memIdx, cl)

--uniqMemIds memIdx (Query p args) = (memIdx, Query p args)
--uniqMemIds memIdx (NegQuery p args) = (memIdx, NegQuery p args)
uniqMemIds memIdx (AndPre pre1 pre2) = (memIdx2, AndPre pre1' pre2')
          where (memIdx1, pre1') = uniqMemIds memIdx pre1
                (memIdx2, pre2') = uniqMemIds memIdx1 pre2
uniqMemIds memIdx (Or pre1 pre2 _) = (memIdx2+1, Or pre1' pre2' memIdx2)
          where (memIdx1, pre1') = uniqMemIds memIdx pre1
                (memIdx2, pre2') = uniqMemIds memIdx1 pre2
uniqMemIds memIdx (ForallPre x pre) = (memIdx', ForallPre x pre')
          where (memIdx', pre') = uniqMemIds memIdx pre
uniqMemIds memIdx (Exist x pre _) = (memIdx'+1, Exist x pre' memIdx')
          where (memIdx', pre') = uniqMemIds memIdx pre
uniqMemIds memIdx pre = (memIdx, pre)

--maxFormulaDepth :: Num a => Formula -> a -> (a -> t) -> t
maxFormulaDepth (Predicate _ _) currDepth maxDepth cont
  | currDepth > maxDepth = cont currDepth currDepth
  | otherwise = cont currDepth maxDepth
maxFormulaDepth (NegPredicate _ _) currDepth maxDepth cont
  | currDepth > maxDepth = cont currDepth currDepth
  | otherwise = cont currDepth maxDepth
maxFormulaDepth (Wedge f1 f2) currDepth maxDepth cont =
  maxFormulaDepth f1 currDepth maxDepth (\currDepth' maxDepth' -> maxFormulaDepth f2 currDepth' maxDepth' cont)
maxFormulaDepth (Vee f1 f2) currDepth maxDepth cont =
  maxFormulaDepth f1 currDepth maxDepth (\currDepth' maxDepth' -> maxFormulaDepth f2 currDepth' maxDepth' cont)
maxFormulaDepth (Rightarrow f1 f2) currDepth maxDepth cont =
  maxFormulaDepth f1 currDepth maxDepth (\currDepth' maxDepth' -> maxFormulaDepth f2 currDepth' maxDepth' cont)
maxFormulaDepth (A _ f) currDepth maxDepth cont = 
  maxFormulaDepth f (currDepth+1) maxDepth (\currDepth' maxDepth' -> cont (currDepth'-1) maxDepth')
maxFormulaDepth (E _ f) currDepth maxDepth cont = 
  maxFormulaDepth f (currDepth+1) maxDepth (\currDepth' maxDepth' -> cont (currDepth'-1) maxDepth')
maxFormulaDepth (Equal _ _) currDepth maxDepth cont = cont currDepth maxDepth
maxFormulaDepth (Nequal _ _) currDepth maxDepth cont = cont currDepth maxDepth
maxFormulaDepth Data.True currDepth maxDepth cont = cont currDepth maxDepth
maxFormulaDepth Data.False currDepth maxDepth cont = cont currDepth maxDepth

maxArity (Predicate _ args) max cont
  | curr > max = cont curr
  | otherwise = cont max 
  where curr = length args
maxArity (NegPredicate _ args) max cont
  | curr > max = cont curr
  | otherwise = cont max 
  where curr = length args
maxArity (Wedge f1 f2) max cont =
  maxArity f1 max (\max' -> maxArity f2 max' cont)
maxArity (Vee f1 f2) max cont =
  maxArity f1 max (\max' -> maxArity f2 max' cont)
maxArity (Rightarrow f1 f2) max cont =
  maxArity f1 max (\max' -> maxArity f2 max' cont)
maxArity (A _ f) max cont = maxArity f max cont
maxArity (E _ f) max cont = maxArity f max cont
maxArity (Equal _ _) max cont = cont max
maxArity (Nequal _ _) max cont = cont max
maxArity Data.True max cont = cont max
maxArity Data.False max cont = cont max

depthPre (AndPre pre1 pre2) = max (depthPre pre1) (depthPre pre2)
depthPre (Or pre1 pre2 _) = max (depthPre pre1) (depthPre pre2)
depthPre (Exist _ pre _) = 1+ depthPre pre
depthPre (ForallPre _ pre) = 1 + depthPre pre
depthPre _ = 0

clauseDepth :: Clause -> Int
clauseDepth (Assertion p args) = 0
clauseDepth (And cl1 cl2) = max (clauseDepth cl1) (clauseDepth cl2)
clauseDepth (Imply pre cl) = max (depthPre pre) (clauseDepth cl)
clauseDepth (Forall x cl) = 1 + (clauseDepth cl)
clauseDepth TT = 0

instance NFData a => NFData (SolverData a) where
  rnf sd = rnf (predicates sd) `seq` rnf (atoms sd) `seq` rnf (cls sd) `seq` ()

instance NFData Clause where
  rnf (Assertion p args) = rnf p `seq` 
                         rnf args `seq`
                         ()
  rnf (And cl1 cl2) = rnf cl1 `seq`
                    rnf cl2 `seq`
                    ()
  rnf (Imply pre cl) = rnf pre `seq`
                     rnf cl `seq`
                     ()
  rnf (Forall x cl) = rnf x `seq`
                    rnf cl `seq`
                    ()
  rnf TT = ()
  
instance NFData Pre where
  rnf (Query p args) = rnf p `seq` 
                       rnf args `seq`
                       ()
  rnf (NegQuery p args) = rnf p `seq` 
                       rnf args `seq`
                       ()  
  rnf (AndPre pre1 pre2) = rnf pre1 `seq`
                    rnf pre2 `seq`
                    ()
  rnf (Or pre1 pre2 idx) = rnf pre1 `seq`
                    rnf pre2 `seq`
                    rnf idx `seq`
                    ()
  rnf (ForallPre x pre) = rnf x `seq`
                          rnf pre `seq`
                          ()
  rnf (Exist x pre idx) = rnf x `seq`
                          rnf pre `seq`
                          rnf idx `seq`
                          ()
  rnf (Eq a1 a2) = rnf a1 `seq`
                   rnf a2 `seq`
                   ()
  rnf (Neq a1 a2) = rnf a1 `seq`
                    rnf a2 `seq`
                    ()
                                        
data Argument = Const Int | Var Int deriving (Show, Read, Eq)

type Arguments = [Argument]

isConst (Var _) = Prelude.False
isConst (Const _) = Prelude.True
isVar (Var _) = Prelude.True
isVar (Const _) = Prelude.False

instance NFData Argument where
  rnf (Const n) = rnf n
  rnf (Var x) = rnf x

--data Solution = Solution { result :: Result, infl :: Infl, memTable :: IntMap Maybe Int }
 
data SolverData a = SolverData { cls :: a, 
                               predicates :: Map.Map String PredData,
                               atoms :: Map.Map String Int,
                               maxPredicateArity :: Int,
                               maxClauseDepth :: Int }
                    deriving(Show)

--createAFLPSolverDataFromFile f = undefined

--createSolverData str = f
 --where f = createClause . formula . alexScanTokens $ str

tryGetId p ps = case Map.lookup p ps of
                 Just idx -> (idx, ps)
                 Nothing -> (n, Map.insert p n ps)
                            where n = Map.size ps

tryGetPredId p pty len ps = 
    case Map.lookup p ps of
      Just pd | isValid -> (pid pd, ps)
              | len /= (parity pd) -> error "Arities do not match."
              | otherwise -> error "Predicate type does not match."
              where isValid = len == (parity pd) 
                            && ((isJust pty && fromJust pty == (ptype pd)) 
                            || isNothing pty)
      Nothing -> (n, Map.insert p pd ps)
                 where n = Map.size ps
                       pd = PredData { pid = n
                                     , parity = len
                                     , ptype = pty' }
                       pty' = case pty of
                                Just t -> t
                                Nothing -> DefPred

getVarIdx x vars = Map.lookup x vars

createArg a vars atoms = case getVarIdx a vars of
                           Just idx -> (Var idx, atoms)
                           Nothing -> (Const newId, newAtoms)
                               where (newId, newAtoms) = tryGetId a atoms

createArgs as vars atoms = foldr f ([], atoms) as
                           where f s (args, atms) = (arg:args, newAtoms)
                                     where (arg, newAtoms) = createArg s vars atms

toPre (Predicate p args) cont td =
      cont (Query pId args') td'
      where (pId, ps') = tryGetPredId p Nothing (length args) (preds td)
            (args', atms') = createArgs args (vars td) (atms td)
            td' = td { preds = ps', atms = atms' }
toPre (NegPredicate p args) cont td = 
      cont (NegQuery pId args') td'
      where (pId, ps') = tryGetPredId p Nothing (length args) (preds td)
            (args', atms') = createArgs args (vars td) (atms td)
            td' = td { preds = ps', atms = atms' }
toPre (Wedge pre1 pre2) cont td =
      toPre pre1 (\p1 -> toPre pre2 (\p2 -> cont (AndPre p1 p2))) td
toPre (Vee pre1 pre2) cont td =
      toPre pre1 (\p1 -> toPre pre2  (\p2 -> cont (Or p1 p2 idx))) td'
      where (idx, td') = getMemIdx td
toPre (E s pre) cont td = 
      toPre pre cont' td'
      where cont' cpre ctd = cont (Exist (getVariableId s td') cpre idx) (deleteVariable s ctd)
            (idx, td') = getMemIdx . addVariable s $ td
toPre (A s pre) cont td = 
         toPre pre  cont' td'
         where cont' cpre ctd = cont (ForallPre (getVariableId s td') cpre) (deleteVariable s ctd)
               td' = addVariable s td
toPre (Equal s1 s2) cont td = 
      cont (Eq arg1 arg2) $ td { atms = atoms'' }
      where (arg1, atoms') = createArg s1 (vars td) (atms td)
            (arg2, atoms'') = createArg s2 (vars td) atoms'
toPre (Nequal s1 s2) cont td = 
      cont (Neq arg1 arg2) $ td { atms = atoms'' }
      where (arg1, atoms') = createArg s1 (vars td) (atms td)
            (arg2, atoms'') = createArg s2 (vars td) atoms'
toPre Data.True cont td = cont TruePre td
toPre Data.False cont td = cont FalsePre td

toClause (Predicate p args) cont td = cont (Assertion pId newArgs) td'
         where (pId, newPs) = tryGetPredId p Nothing (length args) (preds td)
               (newArgs, newAtoms) = createArgs args (vars td) (atms td)
               td' = td { preds = newPs, atms = newAtoms }
toClause (Data.True) cont td = cont TT td
toClause (Wedge cl1 cl2) cont td = 
         toClause cl1 (\c1 -> toClause cl2 (\c2 -> cont (And c1 c2))) td
toClause (Rightarrow pre cl) cont td = 
         toPre pre (\p -> toClause cl (\c -> cont (Imply p c))) td
toClause (A s cl) cont td = 
         toClause cl cont' td'
         where cont' ccl ctd = cont (Forall (getVariableId s td') ccl) (deleteVariable s ctd)
               td' = addVariable s td

data PredType = DefPred | ConPred
     deriving(Show,Eq,Ord)

data PredData = 
     PredData { pid :: Int
              , parity :: Int
              , ptype :: PredType }
--              , prank :: Int }
     deriving(Show,Eq,Ord)         

instance NFData PredData where
  rnf pd = rnf (pid pd) `seq` rnf (parity pd) `seq` ()

isDefPred pd = ptype pd == DefPred
isConPred pd = ptype pd == ConPred

data TranslData =
     TranslData { vars :: Map.Map String Int
                , preds :: Map.Map String PredData
                , atms :: Map.Map String Int
                , nextMemIdx :: Int }

addVariable var td = td { vars = vars' }
                     where vars' = Map.insert var (Map.size . vars $ td) (vars td)
deleteVariable var td = td { vars = vars' }
                        where vars' = Map.delete var (vars td)
getVariableId var td = ((vars td) Map.! var)
getMemIdx td = (idx, td')
               where idx = nextMemIdx td
                     td' = td { nextMemIdx = idx + 1 }
{-
createSolverData filePath = do
  content <- readFile filePath
  return . toSolverData . formula . alexScanTokens $ content
-}

toSolverData :: Formula -> SolverData Clause
toSolverData f = 
  toClause f cont initialTd
  where cont cl td = 
          SolverData { cls = cl
                     , predicates = preds td
                     , atoms = atms td
                     , maxPredicateArity = maxArity f 0 id
                     , maxClauseDepth = maxFormulaDepth f 0 0 (\ x y -> y) }
        initialTd = 
          TranslData { vars = Map.empty
                     , preds = Map.empty
                     , atms = Map.empty
                     , nextMemIdx = 0 }

toClauseList :: Clause -> [Clause]
toClauseList (And cl1 cl2) = cl1' ++ cl2' where
  cl1' = toClauseList cl1
  cl2' = toClauseList cl2
toClauseList cl = [cl]