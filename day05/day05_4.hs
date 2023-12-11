{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (many, (<|>))
import Control.Monad (foldM_, forM_)
import Control.Monad.Trans.RWS (put)

import Data.Attoparsec.Text (Parser, char, decimal, letter, parseOnly, sepBy, skip, skipSpace, string, (<?>))
import Data.List (foldl')
import Data.Text (Text)
import Data.Text.IO qualified as TIO

import Interval (Interval (..), interval, lowerBound)
import PiecewiseMap (Map, invert, refineAndApply)

-- Seed Parsers
parseSeedInterval :: Parser Interval
parseSeedInterval = do
  a <- decimal
  char ' '
  interval a <$> decimal

parseSeedIntervals :: Parser [Interval]
parseSeedIntervals = do
  string "seeds: "
  parseSeedInterval `sepBy` char ' '

-- Map Parsers
parseMap :: Parser Map
parseMap = do
  many (letter <|> char '-')
  string " map:\n"
  parseMapLine `sepBy` char '\n'

parseMapLine :: Parser (Interval, Int)
parseMapLine = do
  d <- decimal
  char ' '
  s <- decimal
  char ' '
  l <- decimal
  return (interval s l, d)

-- Almanac Parser
parseAlmanac :: Parser ([Interval], [Map])
parseAlmanac = do
  seeds <- parseSeedIntervals <?> "Seeds Parser"
  skipSpace
  maps <- ((parseMap <?> "Map Parser") `sepBy` skipSpace) <?> "Maps Parser"
  return (seeds, maps)

flattenSeedIntervals :: [Interval] -> [Int]
flattenSeedIntervals = concatMap (\(Interval s l) -> [s, l])

main :: IO ()
main = do
  inputText <- TIO.getContents
  (seedIntervals, maps) <- case parseOnly parseAlmanac inputText of
    Left err -> error err
    Right success -> return success

  putStrLn "Part a:"
  let seedsA = flattenSeedIntervals seedIntervals
  let seedIntervalsA = map (`interval` 1) seedsA
  let locationIntervalsA = foldl' refineAndApply seedIntervalsA maps
  let locationsA = map lowerBound locationIntervalsA
  print $ minimum locationsA

  putStrLn "Part b:"
  let locationIntervals = foldl' refineAndApply seedIntervals maps
  let minB = minimum $ lowerBound <$> locationIntervals
  print $ minimum $ lowerBound <$> locationIntervals
