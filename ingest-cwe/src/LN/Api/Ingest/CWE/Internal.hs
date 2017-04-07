module LN.Api.Ingest.CWE.Internal (
  parseCWE
) where



import Data.Aeson
import qualified Data.ByteString.Lazy as B

import LN.Api.Ingest.CWE.Types



parseCWE :: FilePath -> IO CWE
parseCWE path = do
  raw <- B.readFile path
  case eitherDecode raw of
    Left e -> error e
    Right v -> pure v
