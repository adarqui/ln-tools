{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Api.Ingest.CWE.Internal (
  parseCWE,
  cwePostDescriptions
) where



import           Control.Monad
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as B
import           Data.Int                   (Int64)
import           Data.Monoid                ((<>))
import           Data.String.Conversions    (cs)
import           Data.Text
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



cwePostDescriptions :: Text -> ByteString -> Int64 -> CWE -> IO ()
cwePostDescriptions api_url api_key resource_id CWE{..} = do
  forM_ weaknesses $ \weakness@CWE_Weakness{..} -> do
    let
      ln_data = case descriptionExtended weaknessDescription of
                  Just extended -> LnDCard $ DCard weaknessName ("Summary:\n"<>(descriptionSummary weaknessDescription)<>"\n\nExtended:\n"<>extended)
                  Nothing       -> LnDCard $ DCard weaknessName (descriptionSummary weaknessDescription)

      tags = [toSafeUrl weaknessName]
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
