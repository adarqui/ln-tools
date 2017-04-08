{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Api.Ingest.CWE.Internal (
  parseCWE,
  cwePostDescriptions,
  cwePostTimeOfIntroduction,
  cwePostConsequences,
  cwePostTerms
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



weaknessToConsequencesBody :: CWE_Weakness -> Maybe Text
weaknessToConsequencesBody weakness@CWE_Weakness{..} =
  case weaknessConsequences of
    Nothing -> Nothing
    Just consequences@CWE_Consequence{..} ->
      case catMaybes [body_scope, body_impact, body_notes] of
        [] -> Nothing
        xs -> Just $ Text.intercalate "\n\n" xs
      where

      body_scope = case (Prelude.filter (/= "Other") $ catMaybes $ Prelude.map bangText consequenceScope) of
        [] -> Nothing
        xs -> Just $ "Scope:\n" <> (Text.intercalate "\n" xs)

      body_impact = case (Prelude.filter (/= "Other") $ catMaybes $ Prelude.map bangText consequenceTechnicalImpact) of
        [] -> Nothing
        xs -> Just $ "Technical Impact:\n" <> (Text.intercalate "\n" xs)

      body_notes = case (Prelude.filter (/= "Other") $ catMaybes $ Prelude.map bangText consequenceNotes) of
        [] -> Nothing
        xs -> Just $ "Notes:\n" <> (Text.intercalate "\n" xs)



weaknessToTimeOfIntroductionBody :: CWE_Weakness -> Text
weaknessToTimeOfIntroductionBody weakness@CWE_Weakness{..} = Text.intercalate "\n" $ Prelude.map timeOfIntroduction weaknessTimeOfIntroduction



weaknessToTermsBody :: CWE_Weakness -> Maybe Text
weaknessToTermsBody weakness@CWE_Weakness{..} =
  case catMaybes [body_alternate, body_notes] of
    [] -> Nothing
    xs -> Just $ Text.intercalate "\n\n" xs
  where
  body_alternate = case Prelude.map term weaknessAlternateTerms of
    [] -> Nothing
    xs -> Just $ "Alternate Terms:\n" <> (Text.intercalate "\n " xs)
  body_notes = case Prelude.map text weaknessTerminologyNotes of
    [] -> Nothing
    xs -> Just $ "TerminologyNotes:\n" <> (Text.intercalate "\n " xs)



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



cwePostConsequences :: Text -> ByteString -> Int64 -> CWE -> IO ()
cwePostConsequences api_url api_key resource_id CWE{..} = do
  forM_ (Prelude.filter (not . Prelude.null . weaknessConsequences) weaknesses) $ \weakness@CWE_Weakness{..} -> do
    case weaknessToConsequencesBody weakness of
      Nothing -> pure ()
      Just body -> do
        let
          ln_data = LnCard $ Card ("CONSEQUENCES: " <> weaknessName) body

          tags = [toSafeUrl weaknessName, "ctx-consequences"]
          leuron_req = defaultLeuronRequest {
            leuronRequestData = ln_data,
            leuronRequestTags = tags
          }
        lr <- runWith (postLeuron_ByResourceId' resource_id leuron_req) defaultApiOpts { apiKey = Just api_key, apiUrl = api_url }
        print lr




cwePostTerms :: Text -> ByteString -> Int64 -> CWE -> IO ()
cwePostTerms api_url api_key resource_id CWE{..} = do
  forM_  weaknesses $ \weakness@CWE_Weakness{..} -> do
    case weaknessToTermsBody weakness of
      Nothing -> pure ()
      Just body -> do
        let
          ln_data = LnCard $ Card ("TERMS: " <> weaknessName) body

          tags = [toSafeUrl weaknessName, "ctx-terms"]
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
