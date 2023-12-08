{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text (Parser, char, decimal, letter, parseOnly, string, skip, skipSpace, sepBy, (<?>))

import Control.Applicative ((<|>), many)
import Control.Monad (foldM_, foldM, forM_)
import Data.Char (isDigit, isSpace)
import Data.List (foldl')
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read qualified as TR
import MultiRangeMap (MultiRangeMap)
import MultiRangeMap qualified as M
import System.Posix (seekDirStream)

-- Parsers
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

parseMap :: Parser (Int -> Int)
parseMap = do
  many (letter <|> char '-')
  string " map:\n"
  lines <- parseMapLine `sepBy` char '\n'
  return $ foldl' (.) id $ map readMapLine lines

parseAlmanac :: Parser ([Int], [Int -> Int])
parseAlmanac = do
  seeds <- parseSeeds <?> "Seeds Parser"
  skipSpace
  maps <- ((parseMap <?> "Map Parser") `sepBy` skipSpace) <?> "Maps Parser"
  return (seeds, maps)

type MyMap = (String, [(Int, Int, Int)], Int -> Int)
parseMyMap :: Parser MyMap
parseMyMap = do
  name <- many (letter <|> char '-')
  string " map:\n"
  lines <- parseMapLine `sepBy` char '\n'
  return $ foldl' (\(name, spec, f) line -> (name, line:spec, f . readMapLine line)) (name, [], id) lines

parseMyAlmanac :: Parser ([Int], [MyMap])
parseMyAlmanac = do
  seeds <- parseSeeds <?> "Seeds Parser"
  skipSpace
  mms <- ((parseMyMap <?> "Map Parser") `sepBy` skipSpace) <?> "Maps Parser"
  return (seeds, mms)

-- Helpers
readMapLine :: (Int, Int, Int) -> (Int -> Int)
readMapLine (d, s, l) x = if s <= x && x < s + l then d + x - s else x

verboseApply :: Int -> (Int -> Int) -> IO Int
verboseApply x f = do
  let y = f x
  putStrLn $ show x ++ " -> " ++ show y
  return y

verboseMap :: Int -> [Int -> Int] -> IO Int
verboseMap = foldM verboseApply

padR :: Int -> String -> String
padR n s
    | length s < n  = s ++ replicate (n - length s) ' '
    | otherwise     = s

verboseMyApply :: Int -> MyMap -> IO Int
verboseMyApply x (name, spec, f) = do
  let y = f x
  putStrLn $ padR 26 name ++ ": " ++ show spec
  putStrLn $ show x ++ " -> " ++ show y
  return y

verboseMyMap :: Int -> [MyMap] -> IO Int
verboseMyMap = foldM verboseMyApply

main :: IO ()
main = do
  inputText <- TIO.getContents
  (seeds, maps) <- case parseOnly parseMyAlmanac inputText of
    Left err -> error err
    Right success -> return success
  let m = foldl' (.) id (map (\(a,b,c) -> c) maps)
  print $ "Seeds:     " ++ show seeds
  print $ "Locations: " ++ show (map m seeds)
  forM_ seeds $ \s -> do
    putStrLn $ "Seed: " ++ show s
    verboseMyMap s maps
    putStrLn ""

  forM_ maps $ \(name, spec, f) -> do
    verboseMyApply 74 (name, spec, f)






-- main :: IO ()
-- main = do
--   lines <- TIO.getContents
--   let (_ : seedsT : seedToSoilT : soilToFertT : fertToWatT : watToLightT : lightToTempT : tempToHumidT : humidToLocT : _) = T.splitOn ":" lines

--       seeds = map readInt $ takeWhile startsWithDigit $ T.words seedsT

--       parseInputGroup :: Text -> [[Int]]
--       parseInputGroup str = fmap (map readInt . T.words) (takeWhile startsWithDigit $ T.lines $ T.tail str)

--       seedToSoil = buildRangeMap $ parseInputGroup seedToSoilT
--       soilToFert = buildRangeMap $ parseInputGroup soilToFertT
--       fertToWat = buildRangeMap $ parseInputGroup fertToWatT
--       watToLight = buildRangeMap $ parseInputGroup watToLightT
--       lightToTemp = buildRangeMap $ parseInputGroup lightToTempT
--       tempToHumid = buildRangeMap $ parseInputGroup tempToHumidT
--       humidToLoc = buildRangeMap $ parseInputGroup humidToLocT

--       lookupSeed s = foldl' M.lookup s [seedToSoil, soilToFert, fertToWat, watToLight, lightToTemp, tempToHumid, humidToLoc]

--   print $ minimum $ map lookupSeed seeds
