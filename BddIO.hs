module BddIO (readFddFile,getQuery) where

import Control.Applicative ( (*>), (<*), (<|>), (<*>), liftA2, pure )
import Debug.Trace

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as B

import Data.Attoparsec.Char8 ( Parser, digit, letter_ascii, satisfy )
import Data.Attoparsec.Combinator
import qualified Data.Attoparsec.Char8 as A

import qualified Data.List as List(map,transpose,concat,foldl',replicate)
import Data.Char

readFddFile :: [Int] -> Int -> Int -> FilePath -> IO [[Int]]
readFddFile univ len startIdx path = do
            contents <- B.readFile path
            return . parseFddFile univ len startIdx $ contents

parseFddFile :: [Int] -> Int -> Int -> ByteString -> [[Int]]
parseFddFile univ len startIdx bs = 
         case A.parseOnly (parseFdd univ len startIdx) bs of
              Right xs -> concatMap (mkTuple [[]]) ys
                    where ys = List.map (\ss -> h ss (startIdx,[])) xs
                          h [] (n,acc) 
                            | n == len+startIdx = acc
                            | otherwise = h [] (n+1,univ:acc)
                          h ((m,zs):xs) (n,acc) 
                            | n == m = h xs (n+1,zs:acc)
                            | n < m = h ((m,zs):xs) (n+1,univ:acc)
                            | otherwise = error "n<m shouldn't happen."
              Left e   -> error e

mkTuple :: [[Int]] -> [[Int]] -> [[Int]]
mkTuple acc [] = acc
mkTuple acc (x:xs) = mkTuple [a:b | a <- x, b <- acc] xs

parseFdd :: [Int] -> Int -> Int -> Parser [[(Int, [Int])]]
parseFdd univ len startIdx = (parseTrue univ len startIdx)
                    <|> parseFalse 
                    <|> many1 (parseParens univ len)

parseParens :: [Int] -> Int -> Parser [(Int, [Int])]
parseParens univ len = A.string strOpen *> (parseTuple univ len) <* A.string strClose

parseTuple :: [Int] -> Int -> Parser [(Int, [Int])]
parseTuple univ len = sepBy1 (parseVals univ len) (parseComma *> A.skipSpace)

parseVals :: [Int] -> Int -> Parser (Int, [Int])
parseVals univ idx = do
          x <- parseDomainIdx
          skipColon
          xs <- sepBy1 parseVal parseSlash
          return (x, xs)

parseDomainIdx :: Parser Int
parseDomainIdx = A.decimal

parseVal :: Parser Int
parseVal = A.decimal

parseComma, parseSlash :: Parser ()
parseComma = A.string strComma *> return ()
parseSlash = A.string strSlash *> return ()

skipColon = A.string strColon *> return ()

parseTrue univ len startIdx = A.string strTrue *> return (mkTrue univ len startIdx)
parseFalse = A.string strFalse *> return []

mkTrue univ len startIdx = [List.map (\x -> (x,univ)) [startIdx..(startIdx+len-1)]]

strOpen, strClose, strComma, strSlash :: ByteString
strOpen = B.pack "<"
strClose = B.pack ">"
strComma = B.pack ","
strSlash = B.pack "/"
strTrue = B.pack "T"
strFalse = B.pack "F"
strColon = B.pack ":"


--Magic Sets parsing.

getQuery str = A.parseOnly parseQuery $ B.pack str

parseQuery :: Parser ([Char], [[Char]])
parseQuery = do
  predicate <- parseIdentifier
  arguments <- A.string (B.pack "(") *> parseArgs <* A.string (B.pack ")")
  return (predicate, arguments)

parseArgs :: Parser [[Char]]
parseArgs = sepBy1 parseIdentifier ((parseComma *> A.skipSpace) <|> parseComma)

parseIdentifier :: Parser [Char]
parseIdentifier = do
  first <- A.anyChar
  rest <- many (letter_ascii <|> digit <|> satisfy (== '_'))
  return $ first:rest