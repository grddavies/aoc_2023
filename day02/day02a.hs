{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Distribution.Compat.Prelude (readMaybe)
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
  deriving (Show)

parseColour :: Text -> Maybe Colour
parseColour str = case str of
  "red" -> Just Red
  "green" -> Just Green
  "blue" -> Just Blue
  _ -> Nothing

countPossible :: Count -> Bool
countPossible x = case colour x of
  Red -> count x <= 12
  Green -> count x <= 13
  Blue -> count x <= 14

gamePossible :: Game -> Bool
gamePossible game = all samplePossible (sets game)
  where
    samplePossible = all countPossible

main :: IO ()
main = TIO.interact $ \text ->
  let parsedGames = map parseGame $ T.lines text
      validGames = catMaybes parsedGames
      invalidLines = length $ filter isNothing parsedGames
   in if invalidLines > 0
        then T.pack $ "Error: Failed to parse " <> show invalidLines <> " line(s)."
        else T.pack $ show $ sum $ map gameId $ filter gamePossible validGames
