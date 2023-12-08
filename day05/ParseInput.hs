{-# LANGUAGE OverloadedStrings #-}

module ParseInput where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text
import Data.List (foldl')
import Data.Text (Text, pack)
import MultiRangeMap (MultiRangeMap)
import MultiRangeMap qualified as M

parseSeeds :: Parser [Int]
parseSeeds = do
  string "seeds: "
  decimal `sepBy` char ' '

parseMapLine :: Parser (Int, Int, Int)
parseMapLine = do
  [d, s, l] <- decimal `sepBy` char ' '
  return (d, s, l)

parseMap :: Parser MultiRangeMap
parseMap = do
  many (letter <|> char '-')
  string " map:\n"
  lines <- parseMapLine `sepBy` char '\n'
  let rm = foldl' M.addRange M.empty lines
  return rm

parseFile :: Parser ([Int], [M.MultiRangeMap])
parseFile = do
  seeds <- "seeds: " *> decimal `sepBy` char ' '
  skipSpace
  maps <- parseMap `sepBy` skipSpace
  return (seeds, maps)
