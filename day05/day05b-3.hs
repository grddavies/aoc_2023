{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text (Parser, char, decimal, letter, parseOnly, string, skip, skipSpace, sepBy, (<?>))

import Control.Applicative ((<|>), many)
import Data.List (foldl')
import Data.Text (Text)
import Data.Text.IO qualified as TIO

import IntervalTree (DIntervalTree)
import qualified IntervalTree as IT

-- Text Parsers for Input
parseSeedPair :: Parser (Int, Int)
parseSeedPair = do
  s <- decimal
  char ' '
  l <- decimal
  return (s, l)

parseSeeds :: Parser [Int]
parseSeeds = do
  string "seeds: "
  pairs <- parseSeedPair `sepBy` char ' '
  return $ concatMap (\(s, l) -> [s..s+l-1]) pairs

parseMapLine :: Parser (Int, Int, Int)
parseMapLine = do
  d <- decimal
  char ' '
  s <- decimal
  char ' '
  l <- decimal
  return (d, s, l)

-- Map Parsers
parseMap :: Parser DIntervalTree
parseMap = do
  many (letter <|> char '-')
  string " map:\n"
  lines <- parseMapLine `sepBy` char '\n'
  return $ IT.fromList lines

parseAlmanac :: Parser ([Int], [DIntervalTree])
parseAlmanac = do
  seeds <- parseSeeds <?> "Seeds Parser"
  skipSpace
  maps <- ((parseMap <?> "Map Parser") `sepBy` skipSpace) <?> "Maps Parser"
  return (seeds, maps)

main :: IO ()
main = do
  inputText <- TIO.getContents
  (seeds, its) <- case parseOnly parseAlmanac inputText of
    Left err -> error err
    Right success -> return success
  let f x = foldl' (flip IT.mapInt) x its
  print $ minimum $ map f seeds