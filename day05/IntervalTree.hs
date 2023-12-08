module IntervalTree (DIntervalTree, Interval, fromList, mapInt, splayLookup, insertTuple, empty, singleton) where

import SplayTree (SplayTree (..), zag, zig)
import SplayTree qualified as ST

type Interval = (Int, Int)
type DIntervalTree = SplayTree Interval Int

splayLookup :: DIntervalTree -> Int-> DIntervalTree
splayLookup Leaf _ = Leaf
splayLookup t@(SplayTree (start, length) _ _ l r) x
  | start <= x && x < start + length = t
  | x < start =
      case splayLookup l x of
        Leaf -> t
        lt -> zig lt t
  | otherwise =
      case splayLookup r x of
        Leaf -> t
        rt -> zag t rt

mapInt :: DIntervalTree -> Int -> Int
mapInt t x =
  let (SplayTree (start, length) dest _ _ _) = splayLookup t x
   in if start <= x && x < start + length
        then dest + x - start
        else x

singleton :: (Interval, Int) -> DIntervalTree
singleton ((start, length), v) = SplayTree (start, length) v 0 Leaf Leaf

empty :: DIntervalTree
empty = ST.empty

insert :: Interval -> Int -> DIntervalTree -> DIntervalTree
insert = ST.insert

insertTuple :: (Int, Int, Int) -> DIntervalTree -> DIntervalTree
insertTuple (dest, start, length) = insert (start, length) dest

-- | /O(n lg n)/. Constructs a splay tree from an unsorted list of key-value pairs.
fromList :: [(Int, Int, Int)] -> DIntervalTree
fromList [] = Leaf
fromList l = foldl (flip insertTuple) Leaf l