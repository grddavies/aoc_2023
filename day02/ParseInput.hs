{-# LANGUAGE OverloadedStrings #-}

module ParseInput where

import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

data Game = Game
  { gameId :: Int,
    sets :: [Sample]
  }
  deriving (Show)

parseGame :: Text -> Maybe Game
parseGame line = do
  let foo = T.dropWhile (not . isDigit) line
  gameId <- readMaybe . T.unpack $ T.takeWhile isDigit foo
  sets <- parseSamples $ T.dropWhile (not . isDigit) $ T.dropWhile (' ' /=) foo
  return Game {gameId = gameId, sets = sets}

type Sample = [Count]

parseSamples :: Text -> Maybe [Sample]
parseSamples str = traverse parseSample $ T.splitOn "; " str

parseSample :: Text -> Maybe Sample
parseSample str = do
  let counts = T.splitOn ", " str
  traverse parseCount counts

data Count = Count
  { count :: Int,
    colour :: Colour
  }
  deriving (Show)

parseCount :: Text -> Maybe Count
parseCount str = do
  let [h, t] = T.splitOn " " str
  count <- readMaybe $ T.unpack h
  colour <- parseColour t
  return Count {count = count, colour = colour}

data Colour = Red | Green | Blue
  deriving (Show, Eq, Ord)

parseColour :: Text -> Maybe Colour
parseColour str = case str of
  "red" -> Just Red
  "green" -> Just Green
  "blue" -> Just Blue
  _ -> Nothing
