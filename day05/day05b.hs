{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text (parseOnly)
import Data.Char (isDigit)
import Data.List (foldl')
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read qualified as TR
import MultiRangeMap (MultiRangeMap)
import MultiRangeMap qualified as M
import ParseInput (parseFile)

splitPairs :: [a] -> [(a, a)]
splitPairs [] = []
splitPairs [x] = error "Uneven list to splitPairs"
splitPairs xs = t : splitPairs bs
  where
    t = (f, s)
    ([f, s], bs) = splitAt 2 xs

main :: IO ()
main = do
  inputText <- TIO.getContents
  (seeds, maps) <- case parseOnly parseFile inputText of
    Left err -> error err
    Right success -> return success

  let lookupSeed s = foldl' M.lookup s maps
      seedRanges = splitPairs seeds
      lookupSeedRange (s, n) = [lookupSeed x | x <- [s .. s + n - 1]]
   in print $ minimum $ concatMap lookupSeedRange seedRanges
