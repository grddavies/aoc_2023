module Interval where

-- Interval of Int defined by start and length
data Interval = Interval Int Int deriving (Eq)

instance Show Interval where
  show :: Interval -> String
  show (Interval s l)
    | l == 1 = "[" ++ show s ++ "]"
    | otherwise = "[" ++ show s ++ ".." ++ show (s + l - 1) ++ "]"

lowerBound :: Interval -> Int
lowerBound (Interval s l) = s

upperBound :: Interval -> Int
upperBound (Interval s l) = s + l - 1

intervalLength :: Interval -> Int
intervalLength (Interval _ l) = l

-- Construct interval from start and length
interval :: Int -> Int -> Interval
interval s l
  | l <= 0 = error $ "Intervals have positive length and this one has start " ++ show s ++ " and length " ++ show l
  | otherwise = Interval s l

-- Construct closed interval from start and end
interval' :: Int -> Int -> Interval
interval' a b = interval a (b - a + 1)

data Relation
  = Equal
  | Disjoint
  | Overlaps
  | Subset
  | Contains
  -- ^ Inverse of 'Subset'.
  | OverlappedBy
  -- ^ Inverse of 'Overlaps'.
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

relation :: Interval -> Interval -> Relation
relation x y
  | ax == ay && bx == by = Equal
  | bx < ay = Disjoint
  | ax > by = Disjoint
  | ax >= ay && bx <= by = Subset
  | ax <= ay && bx >= by = Contains
  | ax < ay && bx <= by = Overlaps
  | ax > ay && bx >= by = OverlappedBy
    where
      ax = lowerBound x
      bx = upperBound x
      ay = lowerBound y
      by = upperBound y

refine :: Interval -> Interval -> [Interval]
refine x y = case relation x y of
  Disjoint -> [x]
  Equal -> [x]
  Contains -> lhs ++ [y] ++ rhs
  Subset -> [x]
  Overlaps -> [interval' ax (ay - 1), interval' ay bx]
  OverlappedBy -> [interval' ax by, interval' (by + 1) bx]
  where
    ax = lowerBound x
    bx = upperBound x
    ay = lowerBound y
    by = upperBound y
    lhs = [interval' ax (ay - 1) | ax /= ay]
    rhs = [interval' (by + 1) bx | bx /= by]
    -- lhs = if ax == ay then [] else [interval' ax (ay - 1)]
    -- rhs = if bx == by then [] else [interval' (by + 1) bx]

toList :: Interval -> [Int]
toList x = [(lowerBound x)..(upperBound x)]

isElementOf :: Int -> Interval -> Bool
isElementOf a x = a >= lowerBound x && a <= upperBound x