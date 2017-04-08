{-# LANGUAGE DeriveGeneric #-}

module LN.Api.Ingest.RFC4949.Types (
  Entry (..)
) where



import           Data.Text
import           Data.Typeable
import           GHC.Generics



data Entry = Entry {
  entryName :: Text,
  entryDef  :: Text
} deriving (Eq, Ord, Show, Read, Generic, Typeable)
