{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)
import Data.List (foldl')
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read qualified as TR
import MultiRangeMap (MultiRangeMap)
import MultiRangeMap qualified as M

splitPairs :: [a] -> [(a, a)]
splitPairs [] = []
splitPairs [x] = error "Uneven list to splitPairs"
splitPairs xs = t : splitPairs bs
  where
    t = (f, s)
    ([f, s], bs) = splitAt 2 xs

startsWithDigit :: Text -> Bool
startsWithDigit = maybe False (isDigit . fst) . T.uncons

readInt :: Text -> Int
readInt str = case TR.decimal str of
  Left _ -> error $ T.unpack $ "Failed to parse '" <> str <> "'"
  Right (x, _) -> x

applyLine :: MultiRangeMap -> [Int] -> MultiRangeMap
applyLine m xs = case xs of
  [drs, srs, n] -> M.addRange m drs srs n
  _ -> error "bad input"

buildRangeMap :: [[Int]] -> MultiRangeMap
buildRangeMap = foldl' applyLine M.empty

main :: IO ()
main = do
  lines <- TIO.getContents
  let (_ : seedsT : seedToSoilT : soilToFertT : fertToWatT : watToLightT : lightToTempT : tempToHumidT : humidToLocT : _) = T.splitOn ":" lines

      seedRanges = splitPairs $ map readInt $ takeWhile startsWithDigit $ T.words seedsT

      parseInputGroup :: Text -> [[Int]]
      parseInputGroup str = fmap (map readInt . T.words) (takeWhile startsWithDigit $ T.lines $ T.tail str)

      seedToSoil = buildRangeMap $ parseInputGroup seedToSoilT
      soilToFert = buildRangeMap $ parseInputGroup soilToFertT
      fertToWat = buildRangeMap $ parseInputGroup fertToWatT
      watToLight = buildRangeMap $ parseInputGroup watToLightT
      lightToTemp = buildRangeMap $ parseInputGroup lightToTempT
      tempToHumid = buildRangeMap $ parseInputGroup tempToHumidT
      humidToLoc = buildRangeMap $ parseInputGroup humidToLocT

      lookupSeed s = foldl' M.lookup s [seedToSoil, soilToFert, fertToWat, watToLight, lightToTemp, tempToHumid, humidToLoc]
      lookupSeedRange (s, n) = [lookupSeed x | x <- [s .. s + n - 1]]

  print $ minimum $ concatMap lookupSeedRange seedRanges
