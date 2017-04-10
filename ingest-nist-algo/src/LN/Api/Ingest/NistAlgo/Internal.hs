{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Api.Ingest.NistAlgo.Internal (
  runEntries,
  runEntry,
  nistPostEntry
) where



import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Lazy         as B
import           Data.Int                     (Int64)
import qualified Data.List                    as List
import           Data.Maybe                   (catMaybes)
import           Data.Monoid
import           Data.String.Conversions
import           Data.Text
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TIO
import           Network.Wreq
import           Prelude
import qualified Prelude                      as P
import           Text.HandsomeSoup
import           Text.XML.HXT.Core

import           LN.Api
import           LN.Api.Ingest.Shared
import           LN.Generate.Default
import           LN.Sanitize.Internal
import           LN.T

import           LN.Api.Ingest.NistAlgo.Types



algoUrl :: String
algoUrl = "https://xlinux.nist.gov/dads/ui.html"



baseUrl :: String
baseUrl = "https://xlinux.nist.gov/dads/"



runEntries :: Text -> ByteString -> Int64 -> IO ()
runEntries api_url api_key resource_id = do
  r <- get algoUrl
  case r ^? responseBody of
    Nothing -> pure ()
    Just v  -> do
      let
        doc   = readString [withParseHTML yes, withWarnings no] $ convertString v
      all_hrefs <- runX $ doc >>> css "a" >>> (getAttrValue "href" &&& Text.XML.HXT.Core.deep getText)
      let
        hrefs = List.filter (\(href,text) -> "HTML" `List.isPrefixOf` href) all_hrefs
      mapM_ (runEntry api_url api_key resource_id) $ List.take 6 hrefs
      pure ()



runEntry :: Text -> ByteString -> Int64 -> (String, String) -> IO (Maybe Entry)
runEntry api_url api_key resource_id (href, name) = do
  r <- get $ baseUrl <> href
  case r ^? responseBody of
    Nothing -> pure Nothing
    Just v  -> do
      let
        doc = readString [withParseHTML yes, withWarnings no] $ convertString v
      body <- runX $ doc >>> css "body" >>> removeAllWhiteSpace //> getText
      let entry = Entry (Text.pack name) (fixText $ P.map Text.pack body)
      print (fixText $ P.map Text.pack body)
      nistPostEntry api_url api_key resource_id entry
      pure $ Just entry



-- | Insanity
-- I'm just doing crazy stuff tonight.
--
fixText :: [Text] -> Text
fixText text = san2
  where
  san1 = List.takeWhile (\x -> not $ "Go to the" `isInfixOf` x) text
  san2 = Text.intercalate "\n" san1



nistPostEntry :: Text -> ByteString -> Int64 -> Entry -> IO ()
nistPostEntry api_url api_key resource_id entry@Entry{..} = do
  let
    ln_data = LnCard $ Card entryName entryDef
    tags = [toSafeUrl entryName, "ctx-full"]
    leuron_req = defaultLeuronRequest {
      leuronRequestData = ln_data,
      leuronRequestTags = tags
    }
  lr <- api api_key api_url (postLeuron_ByResourceId' resource_id leuron_req)
  print lr
