module DataStar where

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Control.DeepSeq
import Text.PrettyPrint.HughesPJ
import Data.Char

--import ConstantPropagation

class Lattice a where
  lub :: a -> a -> a
  glb :: a -> a -> a
  leq :: a -> a -> Bool
  bot :: a
  --top :: a
  top :: a -> a
  compl :: a -> a -> a
  --compl :: a -> a

type PowerSet = Set.Set (IntSet.IntSet)

data Sign
     = Top
     | Plus
     | Minus
     | Zero
     | Bottom
     deriving(Show,Read,Ord,Eq)

instance Lattice Sign where
         leq Bottom _ = True
         leq _ Top = True
         leq _ _ = False
         lub s1 s2
             | s1 `leq` s2 = s2
             | s2 `leq` s1 = s1
             | otherwise = Top
         glb s1 s2
             | s1 `leq` s2 = s1
             | s2 `leq` s1 = s2
             | otherwise = Bottom
         bot = Bottom
         top _ = Top
         compl _ _ = Bottom

data Z
     = MinusInf
     | Z Int
     | PlusInf
     deriving(Show,Ord,Eq)

plus MinusInf PlusInf = undefined
plus PlusInf MinusInf = undefined
plus MinusInf _ = MinusInf
plus _ MinusInf = MinusInf
plus PlusInf _ = PlusInf
plus _ PlusInf = PlusInf
plus (Z x) (Z y) = Z (x+y)

data Interval
     = EmptyInterval
     | Interval (Z, Z)
     deriving(Show,Ord,Eq)

inf (Interval (x, y)) = x
inf EmptyInterval = PlusInf

sup (Interval (x,y)) = y
sup EmptyInterval = MinusInf

lb k z1 z3
   | z1 <= z3 = z1
   | z3 < z1 = if IntSet.null smaller 
               then MinusInf 
               else Z $ IntSet.findMax smaller
               where smaller = IntSet.filter (\x -> (Z x) <= z3) k

ub k z2 z4
   | z4 <= z2 = z2
   | z2 < z4 = if IntSet.null greater
               then PlusInf
               else Z $ IntSet.findMin greater
               where greater = IntSet.filter (\x -> z4 <= (Z x)) k

add i1 i2 = Interval (plus (inf i1) (inf i2), plus (sup i1) (sup i2))

instance Lattice Interval where
         leq i1 i2 = inf i2 <= inf i1 && sup i1 <= sup i2
         --lub (Interval (x1,x2)) (Interval (y1,y2)) = Interval (min x1 y1, min x2 y2)
         --lub EmptyInterval i2 = i2
         --lub i1 _ = i1
         lub EmptyInterval EmptyInterval = EmptyInterval
         lub i1 i2 = Interval (lb k (inf i1) (inf i2), ub k (sup i1) (sup i2))
             where k = IntSet.fromList [0..100]
         bot = EmptyInterval
         top _ = Interval (MinusInf, PlusInf)
         compl all i = undefined
         glb i1 i2 = undefined
{-
beta' str = Interval (I n, I n)
      where n = read str :: Int
betaInv' i = map show . betaInv $ i

beta'' tab x = beta' str
       where str = tab IntMap.! x
-}
instance Analysis Interval where
         beta str = Interval (Z x, Z x)
                    where x = read str ::Int
         betaInv _ = error "betaInv should not be used."
{-         betaInv EmptyInterval = []
         betaInv (Interval (MinusInf, I x)) = map show [x..]
         betaInv (Interval (I x, PlusInf)) = map show [(minBound :: Int)..x]
         betaInv _ = map show [(minBound :: Int)..(maxBound :: Int)] -}

instance Lattice IntSet.IntSet where
         lub s1 s2 = IntSet.union s1 s2
         glb s1 s2 = IntSet.intersection s1 s2
         leq s1 s2 = s1 `IntSet.isSubsetOf` s2
         bot = IntSet.empty
         top all = all
         compl all s = IntSet.difference all s
         --compl s = undefined

instance Analysis IntSet.IntSet where
         --beta a = IntSet.singleton . read $ a
         beta _ = error "beta for IntSet should not be used."
         --betaInv _ = error "betaInv should not be used."
         betaInv s = IntSet.toList $ s

data EnvVal a
     = Atom Int
     | LElem a
     deriving(Show,Read,Ord,Eq)

fromAtom :: EnvVal a -> Int
fromAtom (Atom a) = a
fromAtom _ = error "Env.fromAtom: LElem"

fromLElem :: EnvVal a -> a
fromLElem (LElem l) = l
fromLElem _ = error "Env.fromLElem: Atom"

type Env a = IntMap.IntMap (Maybe (EnvVal a))

class Analysis a where
      beta :: Lattice a => String -> a
      betaInv :: Lattice a => a -> [Int]
--      betaInv :: Lattice a => a -> [String]

instance Analysis Sign where
         beta n
              | x < 0 = Minus
              | x > 0 = Plus
              | otherwise = Zero
            where x = read n
         betaInv _ = error "betaInv should not be used."   
{-         betaInv Minus = ["-1"]
         betaInv Plus = ["1"]
         betaInv Zero = ["0"] -}

data SolverState a = SolverState
     { getResult :: Result a
     , getInfl :: Infl a
     , getMemTab :: MemTab a }

type Result a = IntMap.IntMap (ResultTrie a)
data ResultTrie a
     = NodeR (IntMap.IntMap (ResultTrie a))
     | LeafR a
     deriving (Show, Read, Eq)

type MemTab a = IntMap.IntMap (Set.Set (IntMap.IntMap (Maybe (EnvVal a))))
type Consumer a = Tuple a -> SolverState a -> SolverState a

data InflTrie a = NodeI [Consumer a] (IntMap.IntMap (InflTrie a))
type Infl a = IntMap.IntMap (InflTrie a)

data ArgU
     = ConstU Int
     | VarU Int
     | FunU ([Int] -> Int) [ArgU]

instance Show ArgU where
  show (ConstU c) = "ConstU " ++ (show c)
  show (VarU c) = "VarU " ++ (show c)
  show (FunU f xs) = "FunU" ++ show xs
  
data ArgL a
     = VarL Int
     | AbsL ArgU
     | FunL ([a] -> a) [ArgL a]

instance Show (ArgL a) where
  show (VarL c) = "VarL " ++ (show c)
  show (AbsL (VarU x)) = "[VarU " ++ (show x) ++ "]"
  show (AbsL (ConstU x)) = "[ConstU " ++ (show x) ++ "]"
  show (AbsL x) = "[" ++ show x ++ "]"
  show (FunL f xs) = "FunL" ++ show xs

data Args a
     = BothA [ArgU] (ArgL a)
     | LeftA [ArgU]
     | RightA (ArgL a)
     | EmptyA
     deriving (Show)

isEmptyArgs :: Args a -> Bool
isEmptyArgs EmptyA = True
isEmptyArgs _      = False

isNotEmptyArgs :: Args a -> Bool
isNotEmptyArgs = not . isEmptyArgs

data Pre a
     = Query Int (Args a)
     | NegQuery Int (Args a)
     | Inc Int ArgU
     | AndPre (Pre a) (Pre a)
     | Or (Pre a) (Pre a) Int
     | Exist Int (Pre a) Int
     deriving (Show)
              
data Clause a
     = Assertion Int (Args a)
     | And (Clause a) (Clause a)
     | Imply (Pre a) (Clause a)
     | Forall Int (Clause a)
     | TT
     deriving (Show)
              
data Tuple a
     = BothT [Int] a
     | LeftT [Int]
     | RightT a
     | EmptyT
     deriving (Show)
              
isEmptyTuple :: Tuple a -> Bool
isEmptyTuple EmptyT = True
isEmptyTuple _      = False

isNotEmptyTuple :: Tuple a -> Bool
isNotEmptyTuple = not . isEmptyTuple

data FormulaArgs
     = BothF [ArgFU] ArgFL
     | LeftF [ArgFU]
     | RightF ArgFL
     deriving (Show, Read)

data ArgFU
     = SimpleFU String
     | FunFU String [ArgFU]
     deriving (Show, Read)

data ArgFL
     = VarFL String
     | AbsFL ArgFU
     | FunFL String [ArgFL]
     deriving (Show, Read)

--Consider reusing Tuple type, but paramertize it i.e. Tuple a b = ...
data TupleStr
     = BothTStr [String] String
     | LeftTStr [String]
     | RightTStr String
     | EmptyTStr
     deriving (Read)

instance Show TupleStr where
  show (BothTStr xs l) = "(" ++ showTStr ";" (xs++[l]) ++ ")"
  show (LeftTStr xs) = "(" ++ (showTStr "," xs) ++ ";)"
  show (RightTStr l) = "(;" ++ l ++ ")"
  
showTStr _ [a] = show a
showTStr sep [a,b] = (show a) ++ sep ++ (show b)
showTStr sep (x:xs) = (show x) ++ "," ++ showTStr sep xs

data Formula =
     Predicate String FormulaArgs
     | NegPredicate String FormulaArgs
     | Inclusion String ArgFU
     | Wedge Formula Formula
     | Vee Formula Formula
     | Rightarrow Formula Formula
     | A String Formula
     | E String Formula
     | TrueFormula
     | Equal String String
     | Nequal String String
     deriving (Show, Read)

{-
data WArg a
     = WJust (LArg a)
     | WGlb (WArg a) (WArg a)
     | WCmp (WArg a)
     | WTop
-}

{-
data WArgs a
     = Both [UArg] (WArg a)
     | Left [UArg]
     | Right (WArg a)
-}

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

{-
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
-}

instance NFData (SolverState a) where
  rnf ss = rnf (getResult ss)

instance NFData (ResultTrie a) where
  rnf (NodeR m) = rnf m
  rnf (LeafR l) = ()

{-
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
                                        
instance NFData Argument where
  rnf (Const n) = rnf n
  rnf (Var x) = rnf x

--data Solution = Solution { result :: Result, infl :: Infl, memTable :: IntMap Maybe Int }
-}

data SolverData a =
  SolverData { cls :: Clause a
             , predicates :: Map.Map String (Int, Int)
             , atoms :: Map.Map String Int
             --, atomsInv :: IntMap.IntMap String
             , maxPredicateArity :: Int
             , maxClauseDepth :: Int }
             deriving(Show)
--memTable :: IntMap.IntMap (Set.Set (IntMap.IntMap (Maybe Int))) }
updateTabs
  :: Ord b =>
     IntMap.IntMap b
     -> Map.Map b Int
     -> [Int]
     -> [b]
     -> (IntMap.IntMap b, Map.Map b Int, [Int])
updateTabs idToName nameToId univ xs = foldl f (idToName, nameToId, univ) xs
           where f (t1,t2,ys) s = if Map.member s t2 
                               then (t1, t2, ys) 
                               else (t1', t2', ys') 
                   where n = Map.size t2
                         t2' = Map.insert s n t2
                         t1' = IntMap.insert n s t1
                         ys' = n : ys

tryGetName id tab = case IntMap.lookup id tab of
  Just name -> (name, tab)
  Nothing -> (n, IntMap.insert id n tab)
          where n = show id

tryGetId p ps = case Map.lookup p ps of
                 Just idx -> (idx, ps)
                 Nothing -> (n, Map.insert p n ps)
                            where n = Map.size ps

tryGetId' p len ps = case Map.lookup p ps of
                 Just (idx, arity) | len == arity -> (idx, ps) 
                                   | otherwise -> error "Arities do not match."
                 Nothing -> (n, Map.insert p (n, len) ps)
                            where n = Map.size ps

getVarIdx x vars = Map.lookup x vars

{-
data FormulaArgs
     = BothF [ArgFU] ArgFL
     | LeftF [ArgFU]
     | RightF ArgFL
     deriving (Show, Read)

data ArgFU
     = SimpleFU String
     | FunFU String [ArgFU]
     deriving (Show, Read)

data ArgFL
     = VarFL String
     | AbsFL ArgFU
     | FunFL String [ArgFL]
     deriving (Show, Read)
-}

--functions = Map.empty :: Map.Map String ([a] -> a)
data Funs a
     = FunctU ([Int] -> Int)
     | FunctL ([a] -> a)

--functions = Map.insert "g" (FunctU (\xs -> sum xs)) . Map.singleton "f" $ (FunctL $ foldr1 IntSet.intersection)

functions = Map.empty
--functions = Map.insert "f24" (FunctL (\(s:xs) -> IntMap.insert 1 5 s)) . Map.insert "f32" (FunctL (\(s:xs) -> let n = s IntMap.! 1 in IntMap.insert 1 (n+1) s)) . Map.insert "f23" (FunctL (\(s:xs) -> s)) . Map.insert "init" (FunctL (\xs -> IntMap.empty)) . Map.singleton "f12" $ (FunctL (\(s:xs) -> (IntMap.insert (1::Int) (1::Int) s)))

createArgU (SimpleFU a) vars atoms =
           case getVarIdx a vars of
                Just idx -> (VarU idx, atoms)
                Nothing -> (ConstU newId, newAtoms)
                        where (newId, newAtoms) = tryGetId a atoms

createArgU (FunFU n xs) vars atoms = (FunU f xs', atoms')
           where (xs', atoms') = createArgsU xs vars atoms
                 (FunctU f) = functions Map.! n
{-
createArgU a vars atoms = case getVarIdx a vars of
                           Just idx -> (VarU idx, atoms)
                           Nothing -> (ConstU newId, newAtoms)
                               where (newId, newAtoms) = tryGetId a atoms
-}
createArgL (VarFL l) vars atoms = 
  case getVarIdx l vars of
    Just idx -> (VarL idx, atoms)
    Nothing -> error "Only variables are allowed in the second component."
{-createArgL (AbsFL l) vars atoms =
  case getVarIdx l vars of
    Just idx -> (AbsL $ VarU idx, atoms)
    Nothing -> (AbsL $ ConstU idx', atoms')
  where (idx', atoms') = tryGetId l atoms-}
  
createArgL (AbsFL l) vars atoms = (AbsL l', atoms')
           where (l', atoms') = createArgU l vars atoms
createArgL (FunFL n xs) vars atoms = (FunL f xs', atoms')
           where (xs', atoms') = createArgsL xs vars atoms
                 (FunctL f) = functions Map.! n

createArgsL xs vars atoms =
  foldr f ([], atoms) xs
  where f s (args, atms) = (arg:args, newAtoms)
          where (arg, newAtoms) = createArgL s vars atms

createArgsU xs vars atoms = 
  foldr f ([], atoms) xs
  where f s (args, atms) = (arg:args, newAtoms)
          where (arg, newAtoms) = createArgU s vars atms

createArgs (BothF xs l) vars atoms = (BothA xs' l', atoms'')
                                     where (xs',atoms') = createArgsU xs vars atoms
                                           (l', atoms'') = createArgL l vars atoms'
createArgs (LeftF xs) vars atoms = (LeftA xs', atoms')
                                   where (xs', atoms') = createArgsU xs vars atoms
createArgs (RightF l) vars atoms = (RightA l', atoms')
                                   where (l', atoms') = createArgL l vars atoms

--Replace memTable with unique indexes, sice the map is not used later on.
toPre (Predicate p args) cont vars ps atoms memTable = 
  cont (Query pId newArgs) vars newPs newAtoms memTable
  where (pId, newPs) = tryGetId' p (-1) ps
        (newArgs, newAtoms) = createArgs args vars atoms
toPre (NegPredicate p args) cont vars ps atoms memTable =
  cont (NegQuery pId newArgs) vars newPs newAtoms memTable
  where (pId, newPs) = tryGetId' p (-1) ps
        (newArgs, newAtoms) = createArgs args vars atoms
toPre (Inclusion y u) cont vars ps atoms memTable =
  cont (Inc var a) vars ps atoms' memTable
  where var = case getVarIdx y vars of
                   Just idx -> idx
                   Nothing -> error "Only variables are allowed as Inc." 
        (a, atoms') = createArgU u vars atoms
toPre (Wedge pre1 pre2) cont vars ps atoms memTable = 
  toPre pre1 (\p1 -> toPre pre2 (\p2 -> cont (AndPre p1 p2))) vars ps atoms memTable
toPre (Vee pre1 pre2) cont vars ps atoms memTable =
  toPre pre1 (\p1 -> toPre pre2 (\p2 -> cont (Or p1 p2 memIdx))) vars ps atoms newMemTable
  where memIdx = IntMap.size memTable
        newMemTable = IntMap.insert memIdx Set.empty memTable
toPre (E s pre) cont vars ps atoms memTable =
  toPre pre newCont (Map.insert s (Map.size vars) vars) ps atoms newMemTable
  where newCont ePre vars' ps' atoms' = 
          cont (Exist (vars' Map.! s) ePre memIdx) (Map.delete s vars') ps' atoms' 
        memIdx = IntMap.size memTable
        newMemTable = IntMap.insert memIdx Set.empty memTable

toClause (Predicate p args) cont vars ps atoms memTable =
  cont (Assertion pId newArgs) vars newPs newAtoms memTable
  where (pId, newPs) = tryGetId' p (-1) ps
        (newArgs, newAtoms) = createArgs args vars atoms
toClause (TrueFormula) cont vars ps atoms memTable =
  cont TT vars ps atoms memTable
toClause (Wedge cl1 cl2) cont vars ps atoms memTable =
  toClause cl1 (\c1 -> toClause cl2 (\c2 -> cont (And c1 c2))) vars ps atoms memTable
toClause (Rightarrow pre cl) cont vars ps atoms memTable =
  toPre pre (\p -> toClause cl (\c -> cont (Imply p c))) vars ps atoms memTable
toClause (A s cl) cont vars ps atoms memTable =
  toClause cl newCont (Map.insert s (Map.size vars) vars) ps atoms memTable
  where newCont aCl vars' ps' atoms' =
          cont (Forall (vars' Map.! s) aCl) (Map.delete s vars') ps' atoms'

createClause f =
  toClause f cont Map.empty Map.empty Map.empty IntMap.empty
  --Check if memTable is used here, otherwise remove it.
  where cont cl vars ps atoms memTable = 
          SolverData {cls = cl, 
                      predicates = ps, 
                      atoms = atoms,
                      maxPredicateArity = -1, -- maxArity f 0 id,
                      maxClauseDepth = maxFormulaDepth f 0 0 (\ x y -> y)}
--                      memTable = memTable}
{-
type Body = [HornQuery]
type Head = (String, [String])

data HornQuery
     = HornQuery String [String]
     | HornNegQuery String [String]
     | HornEq String String
     | HornNeq String String
     deriving (Show, Read)

data HornClause = HornClause Head Body

argsToDoc :: [String] -> Doc
argsToDoc ([hd]) = text hd
argsToDoc (hd:tl) = text hd <> comma <> argsToDoc tl
argsToDoc _ = empty

headToDoc :: Head -> Doc
headToDoc (p, args) = text p <> (parens $ argsToDoc args)

hornClauseToDoc :: HornClause -> Doc
hornClauseToDoc (HornClause head []) = (headToDoc head) <> (char '.')
hornClauseToDoc (HornClause head body) = (headToDoc head) <> colon <> (char '-') <> (bodiesToDoc body) <> (char '.')

bodiesToDoc :: Body -> Doc
bodiesToDoc [hd] = hornQueryToDoc hd
bodiesToDoc (hd:tl) = hornQueryToDoc hd <> comma <> bodiesToDoc tl
bodiesToDoc _ = empty

hornClausesToDoc :: [HornClause] -> Doc
hornClausesToDoc = vcat . map hornClauseToDoc
  
hornQueryToDoc :: HornQuery -> Doc
hornQueryToDoc (HornQuery p args) = headToDoc (p, args)
hornQueryToDoc (HornNegQuery p args) = (text "not") <> (parens $ headToDoc (p, args))
hornQueryToDoc (HornEq a1 a2) = text a1 <> equals <> text a2
hornQueryToDoc (HornNeq a1 a2) = text a1 <> text "//" <> equals <> text a2 

toArg :: Set.Set String -> String -> String
toArg vars arg 
  | Set.member arg vars = map toUpper arg
  | otherwise = map toLower arg
                
toArgs :: Set.Set String -> [String] -> [String]
toArgs vars = map (toArg vars)

toHornClause :: Formula -> Set.Set String -> (Set.Set String -> [HornClause] -> [HornClause]) -> [HornClause]
toHornClause (Predicate p args) vars cont = cont vars [HornClause ((map toLower p), toArgs vars args) []]
toHornClause (Wedge f1 f2) vars cont = toHornClause f1 vars (\vars' h1 -> toHornClause f2 vars' (\vars'' h2 -> cont vars'' h1++h2))
toHornClause (Rightarrow f1 f2) vars cont = cont vars clauses
                                       where bodies = map (\f -> toHornBody f vars (\ _ x -> x)) (toDNFList . toDNF $ f1)
                                             --head = toHornHead f2 id
                                             hornClauses = toHornClause f2 vars (\ _ x -> x)
                                             --clauses = map (\body -> HornClause head body++b) bodies
                                             clauses = [(HornClause h $ b1++b2) | b1 <- bodies, (HornClause h b2) <- hornClauses]
toHornClause (A x f) vars cont = toHornClause f (Set.insert x vars) cont
toHornClause Data.True vars cont = cont vars []
toHornClause _ _ _ = error "Not supported clause."

toHornBody :: Formula -> Set.Set String -> (Set.Set String -> Body -> Body) -> Body
toHornBody (Predicate p args) vars cont = cont vars [HornQuery (map toLower p) (toArgs vars args)]
toHornBody (NegPredicate p args) vars cont = cont vars [HornNegQuery (map toLower p) (toArgs vars args)]
toHornBody (Wedge f1 f2) vars cont = toHornBody f1 vars (\vars' h1 -> toHornBody f2 vars' (\vars'' h2 -> cont vars'' h1++h2))
--toHornBody (Vee f1 f2) cont acc = toHornBody f1 (\h1 -> toHornBody f2 (\h2 -> cont $ (h1,h2):acc))
toHornBody (E x f) vars cont = toHornBody f (Set.insert x vars) cont
toHornBody (Equal a1 a2) vars cont = cont vars [HornEq (toArg vars a1) (toArg vars a2)]
toHornBody (Nequal a1 a2) vars cont = cont vars [HornNeq (toArg vars a1) (toArg vars a2)]
toHornBody _ _ _ = error "Not supported body."

toHornHead :: Formula -> Set.Set String -> (Set.Set String -> Head -> Head) -> Head
--toHornHead (Predicate p args) vars cont = cont vars ((map toLower p), (toArgs vars args))
toHornHead (Predicate p args) vars cont = cont vars (p, args)
toHornHead _ _ _ = error "Not supported head."

{-
toDNF :: Formula -> [Formula]
toDNF (Predicate p args) = [Predicate p args]
toDNF (NegPredicate p args) = [NegPredicate p args]
toDNF (Wedge f1 f2) = distr dnf1 dnf2
                      where dnf1 = toDNF f1
                            dnf2 = toDNF f2
toDNF (Vee f1 f2) = dnf1 ++ dnf2
                    where dnf1 = toDNF f1
                          dnf2 = toDNF f2
toDNF _ = error "toDNF: Not implemented."

distr :: [Formula] -> [Formula] -> [Formula]
distr (Vee f1 f2) dnf2 = (distr f1 dnf2) ++ (distr f2 dnf2)
distr dnf1 (Vee f1 f2) = (distr dnf1 f1) ++ (distr dnf1 f2)
distr f1 f2 = [Wedge f1 f2]
-}

toDNF :: Formula -> Formula
toDNF (Predicate p args) = (Predicate p args)
toDNF (NegPredicate p args) = (NegPredicate p args)
toDNF (Wedge f1 f2) = distr dnf1 dnf2
                      where dnf1 = toDNF f1
                            dnf2 = toDNF f2
toDNF (Vee f1 f2) = Vee dnf1 dnf2
                    where dnf1 = toDNF f1
                          dnf2 = toDNF f2
toDNF _ = error "toDNF: Not implemented."

distr :: Formula -> Formula -> Formula
distr (Vee f1 f2) dnf2 = Vee (distr f1 dnf2) (distr f2 dnf2)
distr dnf1 (Vee f1 f2) = Vee (distr dnf1 f1) (distr dnf1 f2)
distr f1 f2 = Wedge f1 f2

toDNFList :: Formula -> [Formula]
toDNFList (Predicate p args) = [Predicate p args]
toDNFList (NegPredicate p args) = [NegPredicate p args]
toDNFList (Wedge f1 f2) = [Wedge f1 f2]
toDNFList (Vee f1 f2) = [f1, f2]
toDNFList _ = error "toDNF: Not implemented."

-}