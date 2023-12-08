{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text (Parser, char, decimal, letter, parseOnly, string, skip, skipSpace, sepBy, (<?>))

import Control.Applicative ((<|>), many)
import Control.Monad (foldM_, foldM, forM_, liftM2)
import Data.Char (isDigit, isSpace)
import Data.List (foldl')
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read qualified as TR
import MultiRangeMap (MultiRangeMap)
import MultiRangeMap qualified as M
import System.Posix (seekDirStream)
import Data.Maybe (fromJust)


-- Map Data Structure
type RangeMap = Int -> Maybe Int
rangeMap :: (Int, Int, Int) -> RangeMap
rangeMap (d, s, l) x = if s <= x && x < s + l then Just (d + x - s) else Nothing

idRangeMap :: RangeMap
idRangeMap = Just

composeRangeMap :: RangeMap -> RangeMap -> RangeMap
composeRangeMap = liftM2 (<|>)

type Map = Int -> Int
runRangeMap :: RangeMap -> Map
runRangeMap f x = fromJust $ f x

type DebugMap = (String, [(Int, Int, Int)], Map)


-- Text Parsers for Input
parseSeeds :: Parser [Int]
parseSeeds = do
  string "seeds: "
  decimal `sepBy` char ' '

parseMapLine :: Parser (Int, Int, Int)
parseMapLine = do
  d <- decimal
  char ' '
  s <- decimal
  char ' '
  l <- decimal
  return (d, s, l)

-- Map Parsers
parseMap :: Parser Map
parseMap = do
  many (letter <|> char '-')
  string " map:\n"
  lines <- parseMapLine `sepBy` char '\n'
  let rm = foldl' (flip composeRangeMap) idRangeMap $ map rangeMap lines
  return $ runRangeMap rm

parseAlmanac :: Parser ([Int], [Map])
parseAlmanac = do
  seeds <- parseSeeds <?> "Seeds Parser"
  skipSpace
  maps <- ((parseMap <?> "Map Parser") `sepBy` skipSpace) <?> "Maps Parser"
  return (seeds, maps)

parseDebugMap :: Parser DebugMap
parseDebugMap = do
  name <- many (letter <|> char '-')
  string " map:\n"
  lines <- parseMapLine `sepBy` char '\n'
  let composeDebug (spec, f) line = (line:spec, composeRangeMap (rangeMap line) f)
  let (specs, f) = foldl' composeDebug ([], idRangeMap) lines
  return (name, specs, runRangeMap f) 

parseDebugAlmanac :: Parser ([Int], [DebugMap])
parseDebugAlmanac = do
  seeds <- parseSeeds <?> "Seeds Parser"
  skipSpace
  mms <- ((parseDebugMap <?> "Map Parser") `sepBy` skipSpace) <?> "Maps Parser"
  return (seeds, mms)

-- Degugging
padR :: Int -> String -> String
padR n s
    | length s < n  = s ++ replicate (n - length s) ' '
    | otherwise     = s

verboseDebugApply1 :: Int -> DebugMap -> IO Int
verboseDebugApply1 x (name, spec, f) = do
  let y = f x
  putStrLn $ padR 26 name ++ ": " ++ show spec
  putStrLn $ show x ++ " -> " ++ show y
  return y

main :: IO ()
main = do
  inputText <- TIO.getContents
  (seeds, maps) <- case parseOnly parseAlmanac inputText of
    Left err -> error err
    Right success -> return success

  let m = foldl' (flip (.)) id maps
  print $ minimum $ map m seeds

debugmain :: IO ()
debugmain = do
  inputText <- TIO.readFile "input"
  (seeds, dmaps) <- case parseOnly parseDebugAlmanac inputText of
    Left err -> error err
    Right success -> return success

  let maps = map (\(_, _, f) -> f) dmaps
  let m = foldl' (flip (.)) id maps
  putStrLn $ "Minimum Location1: " ++ show (minimum $ map m seeds)
  putStrLn ""

  forM_ seeds $ \seed -> do
    putStrLn $ "Seed: " ++ show seed
    foldM_ verboseDebugApply1 seed dmaps