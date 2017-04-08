{-# LANGUAGE OverloadedStrings #-}

module LN.Api.Ingest.RFC4949.Internal (
  parseEntries,
  readTextFile,
  findDollars
) where



import           Data.Monoid
import           Data.Text
import qualified Data.Text as Text
import qualified Data.Text.IO                as TIO
import           Prelude
import qualified Prelude as P

import           LN.Api.Ingest.RFC4949.Types



parseEntries :: FilePath -> IO [Entry]
parseEntries path = do
  contents <- readTextFile path
--  TIO.putStrLn contents
  let dollars = findDollars contents
  pure []



readTextFile :: FilePath -> IO Text
readTextFile path = TIO.readFile path



-- | Ugly stuff
--
findDollars :: Text -> [Entry]
findDollars contents = go (Text.lines contents) []
  where
  go [] accum = P.reverse accum
  go (x:xs) accum =
    let
      block = P.takeWhile (\l -> not $ Text.isPrefixOf "   $ " l) xs
      block_length = P.length block
    in
      if block_length <= 0
        then P.reverse accum
        else go (P.drop block_length xs) $ (Entry (Text.drop 5 x) $ Text.intercalate "\n" block) : accum
