module PiecewiseMap where

import Data.List (foldl')

import Interval

-- Map and related types
type Piece = (Interval, Int)
type Map = [Piece]

-- Assuming that the interval is either equal to or a subset of one piece of the map,
-- or disjoint from all pieces, return the piece if it exists, or Nothing.
findPiece :: Interval -> Map -> Maybe Piece
findPiece x [] = Nothing
findPiece x ((y, d) : rs) = case relation x y of
  Equal -> Just (y, d)
  Subset -> Just (y, d)
  _ -> findPiece x rs

-- Assume that the interval is 
applyMapToInterval :: Map -> Interval -> Interval
applyMapToInterval m x@(Interval s l) = case findPiece x m of
    Nothing -> x
    Just (Interval t _, d) -> Interval (s + d - t) l

refineAndApply :: [Interval] -> Map -> [Interval]
refineAndApply xs m = applyMapToInterval m <$> refineByMap m xs

refineOneByPiece :: Piece -> Interval -> [Interval]
refineOneByPiece (y, _) x = refine x y

refineByMap :: Map -> [Interval] -> [Interval]
refineByMap ps xs = foldl' (\xs p -> concatMap (refineOneByPiece p) xs) xs ps

invert :: Map -> Map
invert [] = []
invert ((Interval s l, d) : rs) = (Interval d l, s) : invert rs
