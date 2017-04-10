{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LN.Api.Ingest.NistAlgo.Internal (
  parseEntries,
  readTextFile,
  findDollars,
  nistPostEntries
) where



import           Control.Monad
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as B
import           Data.Int                   (Int64)
import qualified Data.List                  as List
import           Data.Maybe                 (catMaybes)
import           Data.Monoid
import           Data.Text
import qualified Data.Text as Text
import qualified Data.Text.IO                as TIO
import           Prelude
import qualified Prelude as P

import           LN.Api
import           LN.Generate.Default
import           LN.Sanitize.Internal
import           LN.Api.Ingest.Shared
import           LN.T

import           LN.Api.Ingest.NistAlgo.Types



parseEntries :: FilePath -> IO [Entry]
parseEntries path = do
  contents <- readTextFile path
  pure $ findDollars contents



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
      block = P.map (Text.drop 6) $ P.takeWhile (\l -> not $ Text.isPrefixOf "   $ " l) xs
      block_length = P.length block
    in
      if block_length <= 0
        then go xs accum
        else go (P.drop block_length xs) $ (Entry (Text.drop 5 x) $ fixText block) : accum



-- | Insanity
-- I'm just doing crazy stuff tonight.
--
fixText :: [Text] -> Text
fixText = Text.intercalate "\n"



nistPostEntries :: Text -> ByteString -> Int64 -> [Entry] -> IO ()
nistPostEntries api_url api_key resource_id entries = do
  forM_ entries $ \entry@Entry{..} -> do
    let
      ln_data = LnDCard $ DCard entryName entryDef

      tags = [toSafeUrl entryName, "ctx-definition"]
      leuron_req = defaultLeuronRequest {
        leuronRequestData = ln_data,
        leuronRequestTags = tags
      }
    lr <- api api_key api_url (postLeuron_ByResourceId' resource_id leuron_req)
    print lr
