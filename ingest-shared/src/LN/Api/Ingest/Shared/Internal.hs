{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Api.Ingest.Shared.Internal (
  api,
  defaultApiOpts,
) where



import           Control.Monad
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Lens
import           Data.Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as B
import           Data.Int                   (Int64)
import           Data.Maybe                 (catMaybes)
import           Data.Monoid                ((<>))
import Network.Wreq
import Network.HTTP.Client
import           Data.String.Conversions    (cs)
import           Data.Text
import qualified Data.Text                  as Text
import           Haskell.Api.Helpers        (SpecificApiOptions, defaultSpecificApiOptions)
import           Haskell.Api.Helpers.Shared (ApiError (..), ApiOptions (..),
                                             runWith)

import           LN.Api



api api_key api_url fn = do
  lr <- runWith fn defaultApiOpts { apiKey = Just api_key, apiUrl = api_url }
  print lr



defaultApiOpts :: ApiOptions SpecificApiOptions
defaultApiOpts = ApiOptions {
  apiUrl         = "http://dev.adarq.org",
  apiPrefix      = "api",
  apiKey         = Nothing,
  apiKeyHeader   = Just "x-api-authorization",
  apiOptions     = (defaultSpecificApiOptions & header "User-Agent" .~ ["Mozilla/5.0 (Android 4.4; Mobile; rv:41.0) Gecko/41.0 Firefox/41.0"]),
  apiDebug       = False
}
