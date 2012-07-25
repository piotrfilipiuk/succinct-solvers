module Main where

import Data.List
import System.IO
import System.Environment

limits :: [Int]
--limits = [100, 200, 300, 400, 500, 1000]
limits = [1,2,3,4]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] -> mapM_ (genRDAndSave f) limits
    _   -> error "you have to give a base filename to generate output files"

genRDAndSave filePath limit = writeFile (filePath ++ show limit) phi
  where phi = genRD limit

genRD limit = concat . intersperse (space ++ and' ++ newLine) $ vars ++ [initClause, condTrue] ++ asgns ++ [asgnBack, condFalse, lastAsgn]
  where asgns = map (\x -> genRDAsgn x (x+1)) [1..limit-1]
        asgnBack = genRDAsgn limit 0
        condTrue = genRDCond 0 1
        condFalse = genRDCond 0 (limit+1)
        lastAsgn = genRDAsgn (limit+1) (limit+2)
        vars = ("var(x" ++ (show (limit)) ++ (show 0) ++ ")") : 
               ("var(x" ++ (show (limit+1)) ++ (show (limit+2)) ++ ")") : 
               (map (\x -> "var(x" ++ (show x) ++ (show (x+1)) ++ ")") [1..limit-1])
        initClause = "(A x. var(x) => rd(0,x,undef))"


genRDKill s t = "kill" ++ s' ++ t' ++ "(x" ++ s' ++ t' ++ ")"
  where s' = show s
        t' = show t
        
genRDGen s t = "gen" ++ s' ++ t' ++ "(x" ++ s' ++ t' ++ ",s" ++ s' ++ ")"
  where s' = show s
        t' = show t

genRDClause s t = "(A x. A l. ((rd(s" ++ s' ++ ",x,l) & !kill" ++ s' ++ t' ++ "(x)) | gen" ++ s' ++ t' ++ "(x,l) => rd(s" ++ t' ++ ",x,l)))"
  where s' = show s
        t' = show t

genRDAsgn s t = kill ++ and' ++ newLine ++ gen ++ and' ++ newLine ++ cl
  where kill = genRDKill s t
        gen = genRDGen s t
        cl = genRDClause s t

genRDCond s t = genRDClause s t
genRDSkip s t = genRDClause s t

genAEAndSave filePath limit = writeFile (filePath ++ show limit) phi
  where phi = genAE (2^limit)

{-genAE limit = concat . intersperse (space ++ and' ++ newLine) $ vars ++ [initClause, condTrue] ++ asgns ++ [asgnBack, condFalse, lastAsgn]
  where asgns = map (\x -> genAEAsgn x (x+1)) [1..limit-1]
        asgnBack = genAEAsgn limit 0
        condTrue = genAECond 0 1
        condFalse = genAECond 0 (limit+1)
        lastAsgn = genAEAsgn (limit+1) (limit+2)
        vars = ("expr(x" ++ (show (limit)) ++ (show 0) ++ ")") : 
               ("expr(x" ++ (show (limit+1)) ++ (show (limit+2)) ++ ")") : 
               (map (\x -> "expr(x" ++ (show x) ++ (show (x+1)) ++ ")") [1..limit-1])
        initClause = "(A x. expr(x) => ae(0,x,undef))"-}

genAE n = concat [defineL, constrL]
              where limit = n `div` 2
                    kills = genAEKills limit
                    gens = genAEGens limit
                    transf = map (\x -> genAEClause x (x+1)) [0..limit-2]
                    exprs = map (\x -> "expr(e" ++ (show x) ++ ")") [0..limit-1]
                    states = map (\x -> "state(s" ++ (show x) ++ ")") [0..limit-1]
                    domClause = "(A s. A e. ae(s,e) => (state(s) & expr(e)))"
                    --initClause = "(A e. ae(s0,e) => !expr(e))"
                    initClause = "(A e. ae(s0,e) => expr(e))"
                    defineL = concat $ define : lparen : newLine : (intersperse (space ++ and' ++ newLine) $ states ++ exprs ++ kills ++ gens) ++ [rparen ++ comma ++ newLine]
                    constrL = concat $ constrain : lparen : newLine : (intersperse (space ++ and' ++ newLine) $ domClause:initClause:transf) ++ [rparen]
                    --constrL = concat $ constrain : lparen : newLine : (intersperse (space ++ and' ++ newLine) $ transf) ++ [rparen]

genAEKills limit = map killfun [0..limit-1]
  where killfun n = "kill" ++ n' ++ "(e" ++ m ++ ")"
                    where n' = show n
                          m = show $ limit - n - 1

genAEGens limit = map genfun [0..limit-1]
  where genfun n = "gen" ++ n' ++ "(e" ++ n' ++ ")"
                    where n' = show n

genAEClause s t = "(A e. (ae(s" ++ t' ++ ",e) => (expr(e) & (ae(s" ++ s' ++ ",e) & !kill" ++ s' ++ "(e)) | gen" ++ s' ++ "(e))))"
  where s' = show s
        t' = show t

comma = ","
lparen = "("
rparen = ")"
define = "define"
constrain = "constrain"
newLine = "\n"
and' = "&"
space = " "