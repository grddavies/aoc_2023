{-# LANGUAGE OverloadedStrings #-}

import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.TypeError (ErrorMessage (Text))

parseCard :: Text -> (Set Integer, Set Integer)
parseCard line =
  let (_ : xs : _) = T.splitOn ": " line
      (wins : ours : _) = T.splitOn "|" xs
      slurp t = S.fromList $ map (read . T.unpack) (T.words t)
   in (slurp wins, slurp ours)

main :: IO ()
main = TIO.interact $ \text ->
  let cards = map parseCard $ T.lines text
   in T.pack $ show $ sum $ map ((2 ^) . subtract 1) $ filter (0 <) $ map (S.size . uncurry S.intersection) cards
