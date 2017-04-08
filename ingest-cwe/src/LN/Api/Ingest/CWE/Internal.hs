{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Api.Ingest.CWE.Internal (
  parseCWE,
  cwePostDescriptions,
  cwePostTimeOfIntroduction,
  cwePostConsequences
) where



import           Control.Monad
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as B
import           Data.Int                   (Int64)
import           Data.Maybe                 (catMaybes)
import           Data.Monoid                ((<>))
import           Data.String.Conversions    (cs)
import           Data.Text
import qualified Data.Text                  as Text
import           Haskell.Api.Helpers        (SpecificApiOptions, defaultSpecificApiOptions)
import           Haskell.Api.Helpers.Shared (ApiError (..), ApiOptions (..),
                                             runWith)

import           LN.Api
import           LN.Api.Ingest.CWE.Types
import           LN.Generate.Default
import           LN.Sanitize.Internal
import           LN.T



parseCWE :: FilePath -> IO CWE
parseCWE path = do
  raw <- B.readFile path
  case eitherDecode raw of
    Left e -> error e
    Right v -> pure v



weaknessToDescriptionBody :: CWE_Weakness -> Text
weaknessToDescriptionBody weakness@CWE_Weakness{..} = Text.intercalate "\n\n" $ catMaybes [body_summary, body_extended, platforms]
  where
  body_summary = Just $ "Summary:\n" <> descriptionSummary weaknessDescription
  body_extended = case descriptionExtended weaknessDescription of
              Just extended -> Just $ "Extended:\n" <> extended
              _             -> Nothing
  platforms = case weaknessPlatforms of
                [] -> Nothing
                _  -> Just $ "Platforms:\n" <> (Text.intercalate "," $ Prelude.map platformName weaknessPlatforms)



weaknessToTimeOfIntroductionBody :: CWE_Weakness -> Text
weaknessToTimeOfIntroductionBody weakness@CWE_Weakness{..} = Text.intercalate "\n" $ Prelude.map timeOfIntroduction weaknessTimeOfIntroduction



cwePostDescriptions :: Text -> ByteString -> Int64 -> CWE -> IO ()
cwePostDescriptions api_url api_key resource_id CWE{..} = do
  forM_ weaknesses $ \weakness@CWE_Weakness{..} -> do
    let
      ln_data = LnDCard $ DCard weaknessName (weaknessToDescriptionBody weakness)

      tags = [toSafeUrl weaknessName, "ctx-description"]
      leuron_req = defaultLeuronRequest {
        leuronRequestData = ln_data,
        leuronRequestTags = tags
      }
    lr <- runWith (postLeuron_ByResourceId' resource_id leuron_req) defaultApiOpts { apiKey = Just api_key, apiUrl = api_url }
    print lr



cwePostTimeOfIntroduction :: Text -> ByteString -> Int64 -> CWE -> IO ()
cwePostTimeOfIntroduction api_url api_key resource_id CWE{..} = do
  forM_ (Prelude.filter (not . Prelude.null . weaknessTimeOfIntroduction) weaknesses) $ \weakness@CWE_Weakness{..} -> do
    let
      ln_data = LnCard $ Card ("TIME_OF_INTRODUCTION: " <> weaknessName) (weaknessToTimeOfIntroductionBody weakness)

      tags = [toSafeUrl weaknessName, "ctx-time-of-introduction"]
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
