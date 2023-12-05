{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)
import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

parseCard :: Text -> (Int, (Set Int, Set Int))
parseCard line =
  let (hd : xs : _) = T.splitOn ": " line
      (wins : ours : _) = T.splitOn "|" xs
      cardNo = read . takeWhile isDigit $ dropWhile (not . isDigit) (T.unpack hd)
      slurp t = S.fromList $ map (read . T.unpack) (T.words t)
   in (cardNo, (slurp wins, slurp ours))

countCopies :: Int -> (Set Int, Set Int) -> Map Int Int -> Map Int Int
countCopies key (s1, s2) m =
  let w = S.size (s1 `S.intersection` s2) -- w winning cards
      n = fromMaybe 0 (M.lookup key m) -- n copies of this card
      go 0 m = m
      go i m = go (i - 1) $ M.insertWith (+) (key + i) n m
   in go w m

calculateTotal :: [(Int, (Set Int, Set Int))] -> Int
calculateTotal cards =
  sum $ M.elems $ foldl' (\m (card, sets) -> countCopies card sets m) counts cards
  where
    counts = M.fromList $ map ((,1) . fst) cards

main :: IO ()
main = do
  text <- TIO.getContents
  let cards = map parseCard $ T.lines text
      counts = M.fromList $ map ((,1) . fst) cards
  print $ calculateTotal cards
