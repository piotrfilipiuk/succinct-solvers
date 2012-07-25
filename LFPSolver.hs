{-# Language GADTs #-}
module LFPSolver where

import qualified Data.IntMap as IntMap(IntMap,lookup,empty,delete,insert,(!))
import qualified Data.Set as Set(Set,member,insert,singleton)
import List(nub)
import Data
import BddUtils

import qualified Data.Map as Map(Map,elems,fold)
import Control.Monad

import LFPParser
import LFPLexer

import LFPLogic
import BddRelation
import Bdd
import Fdd
import Foreign.C.Types


solveFromFile filePath = do
  content <- readFile filePath
  solveFromString $ content

solveFromString :: String -> IO (Map.Map String [[String]])
solveFromString s = do
                let solverData = createSolverData s
                    univ = Map.elems (Data.atoms solverData)
                    getArty pd acc = IntMap.insert (pid pd) (parity pd) acc
                    arities = Map.fold getArty IntMap.empty (predicates solverData)
                bdd_init 100000 10000
                fdd_extdomain . fddDomains $ solverData
                result <- solve solverData
                result' <- bddsToTuples univ arities (maxClauseDepth solverData) result
                bdd_done
                return . fromIntMapResult solverData $ result'

createSolverData :: String -> SolverData [LFPLayer]
createSolverData str = f
 where f = toLFP . layerFormula . alexScanTokens $ str

initInfl = Map.fold f (IntMap.empty)
  where f pd acc = IntMap.insert (pid pd) (InflSet []) acc

initResult = Map.fold f (IntMap.empty)
  where f pd acc = IntMap.insert (pid pd) val acc
                   where val = case ptype pd of
                                 DefPred -> 0
                                 ConPred -> 1

solve :: SolverData [LFPLayer] -> IO Result
solve sd = do
  ss <- foldM doit initState $ (cls sd) 
  return . result $ ss where
  doit st layer = do
       env <- bddtrue
       execute (getClause layer) (env, st)
  initState = BddSSt { result = initialResult
                     , infl = initialInfl
                     , memTab = IntMap.empty }
  initialInfl = initInfl (predicates sd)
  initialResult = initResult (predicates sd)
  clauseDepth = fromIntegral . maxClauseDepth $ sd
  checkPredicate args cont env r ss = do
    envcp <- bdd_addref env
    envf <- product' envcp r >>= doFold
    envp <- projectOut projdoms envf
    bdd_delref envcp
    bdd_delref envf
    cont (envp, ss)
    where doFold eps = foldM f eps zipped
          f acc (i,x) = do
            result <- select x i acc
            bdd_delref acc
            return result
          zipped = zip [clauseDepth..] args
          arity = length $ args
          projdoms = [clauseDepth..clauseDepth+(fromIntegral arity)]
  check (Query p args) cont (env, ss) = do
    --print (Query p args)
    --fdd_printset env
    checkPredicate args cont env (getContent p ss') ss'
    where ss' = register p (checkPredicate args cont env) ss
  check (NegQuery p args) cont (env, ss) = do
    --print (NegQuery p args)
    --fdd_printset env
    r <- complement . getContent p $ ss
    checkPredicate args cont env r ss
    --bdd_delref r
  check (AndPre pre1 pre2) cont (env, ss) =
    check pre1 cont' (env, ss)
    where cont' = check pre2 cont
  check (Or pre1 pre2 _) cont (env, ss) = 
    check pre1 cont1 (env, ss)
    where cont1 (env1, ss1) = 
            check pre2 cont2 (env,ss1)
            where cont2 (env2, ss2) = do
                  env_union <- union env1 env2
                  cont (env_union,ss2)
  check (Exist var pre _) cont (env, ss) = do
    env' <- exist (fromIntegral var) env
    --bdd_delref env
    check pre cont' (env', ss)
    --bdd_delref env'
    where cont' (env',ss') = do
            env_exist <- exist (fromIntegral var) env'
            --bdd_delref e'
            cont (env_exist,ss')
            -- bdd_delref env_exist should be called
  check (ForallPre var pre) cont (env, ss) =
    check pre cont' (env, ss)
    where cont' (env',ss') = do
            env_forall <- forall var env'
            --bdd_delref e'
            cont (env_forall,ss')
            -- bdd_delref env_forall should be called
  check (Eq (Const c1) (Const c2)) cont (env, ss)
    | c1 == c2 = cont (env, ss)
    | otherwise = return ss
  check (Eq u v) cont (env, ss) = do
    env' <- select' u v env
    --bdd_delref env
    cont (env', ss)
    -- bdd_delref env' should be called
  check (Neq (Const c1) (Const c2)) cont (env, ss)
    | c1 /= c2 = cont (env, ss)
    | otherwise = return ss
  check (Neq u v) cont (env, ss) = do
    env1 <- select' u v env
    env2 <- complement env1
    env' <- intersection env env2
    --bdd_delref env
    bdd_delref env1
    bdd_delref env2
    cont (env', ss)
    -- bdd_delref env_exist should be called
  check TruePre cont (env, ss) = cont (env, ss)
  check FalsePre cont (env, ss) = do
    --bdd_delref env 
    ff <- bddfalse
    cont (ff, ss)
  execute (Assert (Pred p xs)) (env, st) = assert (Pred p xs) (env, st)
  execute (Constr (Pred p xs)) (env, st) = do
    ff <- bddfalse
    constrain (Pred p xs) (ff, st)
  execute (AndClause cl1 cl2) (env, ss) = do
    ss' <- execute cl1 (env, ss)
    execute cl2 (env, ss')
  execute (DefImply pre d) (env, ss) =
    check pre cont' (env, ss)
    where cont' = assert d
  execute (ConImply c pre) (env, ss) = do
    check pre cont' (env, ss)
    where cont' = constrain c
  execute (ForAll var cl) (env, ss) = do
    --env' <- exist (fromIntegral var) env
    execute cl (env, ss)
  assert (LFPLogic.Pred p args) (0, ss) = return ss
  assert (LFPLogic.Pred p args) (env, ss) = do
    envcp <- bdd_addref env
    bddf <- foldM f envcp zipped
    bddp <- projectOut [0..clauseDepth-1] bddf
    bdd_delref bddf
    new <- union old bddp
    bdd_delref bddp
    if old /= new 
      then do
           bdd_delref old
           let ss' = insert p new ss
           resume p new ss'
      else return ss
    where old = getContent p ss
          zipped = zip [clauseDepth..] args
          f acc (i,x) = do
            result <- select x i acc
            bdd_delref acc
            return result
  constrain (LFPLogic.Pred p args) (eps, ss) = do
    epsc <- complement eps
    unif <- mkCndBdd epsc
    unifc <- complement unif
    new <- intersection unifc old
    bdd_delref unif
    bdd_delref unifc
    if old /= new
      then do
           bdd_delref old
           let ss' = insert p new ss
           resume p new ss'
      else return ss
    where old = getContent p ss
          zipped = zip [clauseDepth..] args
          mkCndBdd env = do
            inter <- foldM f env zipped
            projectOut [0..clauseDepth-1] inter
          f acc (i,x) = do
            result <- select x i acc
            bdd_delref acc
            return result