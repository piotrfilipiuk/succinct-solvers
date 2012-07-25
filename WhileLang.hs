{-# LANGUAGE GADTs, StandaloneDeriving #-}
module WhileLang where

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

import DataStar
--import SolverStar
--import OutputStar(printSolution)

type Var = Int

data Expr a b where
     I   :: Int  -> Expr Int b
     B   :: Bool -> Expr Bool b
     V   :: b -> Expr Int b
     Add :: Expr Int b -> Expr Int b -> Expr Int b
     Sub :: Expr Int b -> Expr Int b -> Expr Int b
     Div :: Expr Int b -> Expr Int b -> Expr Int b
     Mul :: Expr Int b -> Expr Int b -> Expr Int b
     Eq  :: Expr Int b -> Expr Int b -> Expr Bool b
     Neq :: Expr Int b -> Expr Int b -> Expr Bool b
     Not :: Expr Bool b -> Expr Bool b

deriving instance (Show b) => Show (Expr a b)

{-
instance Show (Expr a b) where
         show (I n) = "I " ++ (show n)
         show (V n) = "V " ++ (show n)
         show (Add e1 e2) = (show e1) ++ " + " ++ (show e2)
-}

data Stmt a b
     = Asgn b (Expr Int b)
     | Skip
     | While (Expr Bool b) (Stmt a b)
     | If (Expr Bool b) (Stmt a b) (Stmt a b)
     | Semi (Stmt a b) (Stmt a b)
     deriving(Show)

{-
data Stmt a b where
     Asgn :: b -> Expr Int b -> Stmt a b
     Skip :: Stmt a b
     While :: Expr Bool b -> Stmt a b -> Stmt a b
     If :: Expr Bool b -> Stmt a b -> Stmt a b -> Stmt a b
     Semi :: Stmt a b -> Stmt a b -> Stmt a b
-}
--transStmt :: Stmt t a -> Map.Map a Int -> Stmt t Int
transStmt :: Ord k => Stmt a k -> Map.Map k Int -> (Stmt a Int, Map.Map k Int)
transStmt (Asgn s e) vars = (Asgn id e', vars')
          where (id, vars') = case Map.lookup s vars of
                         Just x -> (x, vars)
                         Nothing -> (y, Map.insert s y vars)
                                 where y = Map.size vars
                e' = transExpr e vars'
transStmt Skip vars = (Skip, vars)
transStmt (While e s) vars = (While e' s', vars')
          where e' = transExpr e vars
                (s', vars') = transStmt s vars
transStmt (If e s1 s2) vars = (If e' s1' s2', vars'')
          where e' = transExpr e vars
                (s1', vars') = transStmt s1 vars
                (s2', vars'') = transStmt s2 vars'
transStmt (Semi s1 s2) vars = (Semi s1' s2', vars'')
          where (s1', vars') = transStmt s1 vars
                (s2', vars'') = transStmt s2 vars'

--transExpr = undefined
transExpr :: Ord k => Expr a k -> Map.Map k Int -> Expr a Int
transExpr (I n) _ = I n
transExpr (B b) _ = B b
transExpr (V v) vars = V n
          where n = vars Map.! v
transExpr (Add e1 e2) vars = Add e1' e2'
          where e1' = transExpr e1 vars
                e2' = transExpr e2 vars
transExpr (Sub e1 e2) vars = Sub e1' e2'
          where e1' = transExpr e1 vars
                e2' = transExpr e2 vars
transExpr (Mul e1 e2) vars = Mul e1' e2'
          where e1' = transExpr e1 vars
                e2' = transExpr e2 vars
transExpr (Div e1 e2) vars = Div e1' e2'
          where e1' = transExpr e1 vars
                e2' = transExpr e2 vars
transExpr (Neq e1 e2) vars = Neq e1' e2'
          where e1' = transExpr e1 vars
                e2' = transExpr e2 vars
transExpr (Eq e1 e2) vars = Eq e1' e2'
          where e1' = transExpr e1 vars
                e2' = transExpr e2 vars

eval :: Expr a Int -> IntMap.IntMap Int -> a
eval (I n) _ = n
eval (B b) _ = b
eval (V v) env = env IntMap.! v
eval (Add e1 e2) env = eval e1 env + eval e2 env
eval (Mul e1 e2) env = eval e1 env * eval e2 env
eval (Sub e1 e2) env = eval e1 env - eval e2 env
eval (Div e1 e2) env = eval e1 env `div` eval e2 env
eval (Eq  e1 e2) env = eval e1 env == eval e2 env
eval (Neq  e1 e2) env = eval e1 env /= eval e2 env

data Action
     = AsgnAct Var (Expr Int Int)
     | SkipAct
     | TestAct (Expr Bool Int)
     deriving(Show)

--createPG :: Stmt a b -> ([(Int, Action, Int)], Int)
createPG stmt = f stmt 0 []
  where f (Asgn x e) loc acc = ((loc, (AsgnAct x e), loc+1) : acc, loc+1)
        f Skip loc acc = ((loc, SkipAct, loc+1) : acc, loc+1)
        f (While b s) loc acc = (acc', (loc'+1))
          where (xs, loc') = f s (loc+1) acc
                acc' = (loc, (TestAct b), loc+1) : 
                       (loc, (TestAct $ Not b), loc'+1) : 
                       (loc', SkipAct, loc) : 
                       xs
        f (If b s1 s2) loc acc = (acc', loc2')
          where (xs1, loc1') = f s1 (loc+1) acc
                (xs2, loc2') = f s2 (loc1'+1) xs1
                acc' = (loc, TestAct b, loc+1) :
                       (loc, TestAct $ Not b, (loc1'+1)) :
                       (loc1', SkipAct, loc2') :
                       xs2
        f (Semi s1 s2) loc acc = f s2 loc' xs1
          where (xs1, loc') = f s1 loc acc

trFunCP (AsgnAct x e) env = IntMap.insert x v env
        where v = eval e env
trFunCP SkipAct env = env
trFunCP (TestAct e) env = env

generateCP stmt =
           SolverData { cls = And initClause cls, 
                        predicates = predicates, 
                        atoms = atoms,
                        maxPredicateArity = maxPredicateArity,
                        maxClauseDepth = maxClauseDepth }
           where (stmt', vars) = transStmt stmt Map.empty
                 (xs, loc) = createPG stmt'
                 cls = genClsCP . reverse $ xs
                 atoms = foldl (\acc x -> Map.insert (show x) x acc) Map.empty [0..loc]
                 predicates = Map.singleton "CP" (predId,2)
                 maxPredicateArity = 2
                 maxClauseDepth = 1
                 predId = 0
                 initClause = Assertion predId (BothA [ConstU 0] (FunL (\xs -> IntMap.empty) []))
                 genClsCP [] = TT
                 genClsCP ((q1, act, q2):xs) = And cl (genClsCP xs)
                   where varId = 0
                         cl = Forall varId (Imply pre cl')
                         pre = Query predId (BothA [ConstU q1] (VarL varId))
                         cl' = Assertion predId (BothA [ConstU q2] (FunL (\(x:xs) -> trFunCP act x) [VarL varId]))

--test = generateCP (Semi (Asgn 1 (I 10)) (Asgn 2 (I 22)))

{-
doCP s = printSolution ss solverData
  where solverData = generateCP . program . alexScanTokens $ s
        universe = Map.elems (atoms solverData)
        ss = solve (cls solverData) universe topL complL beta'
        preds = map (\ (p,_) -> p) . Map.elems . predicates $ solverData
        topL = top IntMap.empty
        complL = compl IntMap.empty
        beta' x = error "beta' for CP should not be used."
-}