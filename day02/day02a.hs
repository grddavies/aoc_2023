{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (catMaybes, isNothing)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import ParseInput

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
