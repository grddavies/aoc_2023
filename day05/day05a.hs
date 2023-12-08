import Data.Attoparsec.Text (parseOnly)
import Data.List (foldl')
import Data.Text.IO qualified as TIO
import MultiRangeMap qualified as M
import ParseInput (parseFile)

main :: IO ()
main = do
  inputText <- TIO.getContents
  (seeds, maps) <- case parseOnly parseFile inputText of
    Left err -> error err
    Right success -> return success

  let lookupSeed s = foldl' M.lookup s maps
   in print $ minimum $ map lookupSeed seeds
