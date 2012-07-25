module Main(main) where

import System.Console.GetOpt
import System(ExitCode(ExitSuccess),getProgName,getArgs,exitFailure,exitWith)
import Control.Monad(when)
import IO(hPutStrLn,stderr,hFlush)

import qualified BddSolver as BddSolver(solveFromString)
import qualified LFPSolver as LFPSolver(solveFromString)

data Opt = Opt
     { optInput :: IO String
     , optOutput :: String -> IO ()
     , optRun :: String -> IO String
     }

startOpt :: Opt
startOpt = Opt
         { optInput = getContents
         , optOutput = putStr
         , optRun = run LFPSolver.solveFromString
         }

options :: [OptDescr (Opt -> IO Opt)]

options =
        [ Option "h" ["help"]
                 (NoArg (\opt -> exitHelp)) 
                 "Show usage info"
        , Option "i" ["input"]
                 (ReqArg inputFun "FILE")
                 "Input file, - for stdin"
        , Option "o" ["output"]
                 (ReqArg outputFun "FILE")
                 "Output file"
        , Option "s" ["string"]
                 (ReqArg
                   (\arg opt -> return opt { optInput = return arg })
                 "FILE")
                 "Input string"
        , Option "a" ["alfp"]
                 (NoArg (\opt -> return opt { optRun = run BddSolver.solveFromString }))
                 "ALFP solver"
        , Option "l" ["lfp"]
                 (NoArg (\opt -> return opt { optRun = run LFPSolver.solveFromString }))
                 "LFP solver"
        ]

inputFun arg opt = do
         return opt { optInput =
                        case arg of
                          "-" -> getContents
                          _   -> readFile arg }

outputFun arg opt = do
          return opt { optOutput =
                         case arg of
                           "-" -> putStr
                           _   -> writeFile arg }

run f s = do
    res <- f s
    return . show $ res

main = do
     (opts, _) <- parseOptions
     let Opt { optInput = input
             , optOutput = output
             , optRun = doit } = opts
     input >>= doit >>= output

exitHelp :: IO a
exitHelp = do
         showHelp
         exitWith ExitSuccess

showHelp :: IO ()
showHelp = do
         prg <- getProgName
         hPutStrLn stderr (usageInfo prg options)
         hFlush stderr

parseOptions :: IO (Opt, [String])
parseOptions = do
             (optsActions, rest, errors) <- getArgs >>= return . getOpt RequireOrder options
             when (not (null errors)) $ do
                  mapM_ (hPutStrLn stderr) errors
                  showHelp
                  exitFailure
             opts <- foldl (>>=) (return startOpt) optsActions
             return (opts, rest)