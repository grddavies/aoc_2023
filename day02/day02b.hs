{-# LANGUAGE OverloadedStrings #-}

import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes, isNothing)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import ParseInput

maxCounts :: [Count] -> Map Colour Int
maxCounts = foldr (\x -> M.insertWith max (colour x) (count x)) M.empty

main :: IO ()
main = TIO.interact $ \text ->
  let parsedGames = map parseGame $ T.lines text
      validGames = catMaybes parsedGames
      invalidLines = length $ filter isNothing parsedGames
   in if invalidLines > 0
        then T.pack $ "Error: Failed to parse " <> show invalidLines <> " line(s)."
        else T.pack $ show $ sum $ map (M.foldr (*) 1 . maxCounts . concat . sets) validGames
