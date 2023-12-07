module MultiRangeMap where

import Data.Foldable (find)
import Data.List (foldl')

m = MultiRangeMap [((0, 49), (0, 49)), ((50, 97), (52, 99))]

empty = mempty :: MultiRangeMap

data MultiRangeMap where
  MultiRangeMap :: {ranges :: [RangeMap]} -> MultiRangeMap
  deriving (Show)

instance Semigroup MultiRangeMap where
  MultiRangeMap xs <> MultiRangeMap ys = MultiRangeMap $ foldl' (flip insert) xs ys

instance Monoid MultiRangeMap where
  mempty = MultiRangeMap []

type Range = (Int, Int)

type RangeMap = (Range, Range)

inRange :: Int -> Range -> Bool
inRange x r = x >= fst r && x <= snd r

lookup :: Int -> MultiRangeMap -> Int
lookup k m =
  let x = find (\t -> k `inRange` fst t) $ ranges m
   in maybe k (\(i, o) -> k - fst i + fst o) x

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys) =
  if x <= y then x : y : ys else y : insert x ys

addRange m d s n =
  -- WARN: Assumes no colliding ranges
  let r = ((s, s + n - 1), (d, d + n - 1))
   in MultiRangeMap {ranges = insert r (ranges m)}
