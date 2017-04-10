{-# LANGUAGE DeriveGeneric #-}

module LN.Api.Ingest.NistAlgo.Types (
  Entry (..)
) where



import           Data.Text
import           Data.Typeable
import           GHC.Generics



data Entry = Entry {
  entryName :: Text,
  entryDef  :: Text
} deriving (Eq, Ord, Show, Read, Generic, Typeable)
