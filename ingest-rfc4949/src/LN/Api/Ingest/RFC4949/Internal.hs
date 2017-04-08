{-# LANGUAGE OverloadedStrings #-}

module LN.Api.Ingest.RFC4949.Internal (
  parseEntries,
  readTextFile,
  findDollars,
  rfcPostEntries
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

import           Haskell.Api.Helpers        (SpecificApiOptions, defaultSpecificApiOptions)
import           Haskell.Api.Helpers.Shared (ApiError (..), ApiOptions (..), runWith)
import           LN.Api
import           LN.Generate.Default
import           LN.Sanitize.Internal
import           LN.T

import           LN.Api.Ingest.RFC4949.Types



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
fixText text = san7
  where
  san1 = P.map P.head $ List.groupBy (\x y -> x == y && Text.null x) text
  san2 = List.foldl' (\acc s -> go acc s) "" san1
  san4 = Text.unlines $ P.map P.head $ List.groupBy (\x y -> x == y && Text.null x) $ Text.lines san2
  san5 = Text.dropWhileEnd (=='\n') san4
  san6 = Text.pack $ List.unlines $ P.map (List.unwords . List.words) $ List.lines $ Text.unpack san5
  san7 = Text.dropWhileEnd (=='\n') san6
  go acc s =
    case (Text.null acc, s) of
         (True, _) -> s <> " "
         (_, "")   -> acc <> "\n\n"
         _         ->
           if Text.last s == '.' || Text.last s == ':'
              then acc <> s <> "\n"
              else acc <> s <> " "



fixText' :: [Text] -> Text
fixText' text =
  List.foldl' go "__initial__" l
  where
  l = P.map P.head $ List.groupBy (\a b -> a == b && Text.null a) text
  go acc s = case acc of
                  "__initial__" -> s
                  _ ->
                    if Text.null (Text.replace " " "" s)
                       then acc <> "\n\n"
                       else case (Text.uncons $ Text.reverse acc) of
                                 Nothing       -> acc <> " " <> s
                                 Just (h, t) -> if h == '\n'
                                                   then acc <> s
                                                   else acc <> " " <> s



rfcPostEntries :: Text -> ByteString -> Int64 -> [Entry] -> IO ()
rfcPostEntries api_url api_key resource_id entries = do
  forM_ entries $ \entry@Entry{..} -> do
    let
      ln_data = LnDCard $ DCard entryName entryDef

      tags = [toSafeUrl entryName, "ctx-definition"]
      leuron_req = defaultLeuronRequest {
        leuronRequestData = ln_data,
        leuronRequestTags = tags
      }
    lr <- runWith (postLeuron_ByResourceId' resource_id leuron_req) defaultApiOpts { apiKey = Just api_key, apiUrl = api_url }
    print lr



defaultApiOpts :: ApiOptions SpecificApiOptions
defaultApiOpts = ApiOptions {
  apiUrl         = "http://dev.adarq.org",
  apiPrefix      = "api",
  apiKey         = Nothing,
  apiKeyHeader   = Just "x-api-authorization",
  apiOptions     = defaultSpecificApiOptions,
  apiDebug       = False
}
