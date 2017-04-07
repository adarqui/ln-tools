{-# LANGUAGE OverloadedStrings #-}

module LN.Api.Ingest.CWE.Types (
  CWE (..)
) where



import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Text
import           Data.Typeable
import           GHC.Generics



data CWE = CWE {
  catalogDate :: Text,
  catalogVersion :: Text,
  catalogName :: Text,
  categories :: [CWE_Category],
  weaknesses :: [CWE_Weakness]
} deriving (Eq, Show, Read, Generic, Typeable)



data CWE_Category = CWE_Category {
  categoryStatus :: Text,
  categoryID :: Text,
  categoryName :: Text,
  categoryDescription :: CWE_Description
  -- categoryApplicationPlatform
  -- categoryTimeOfIntroduction
} deriving (Eq, Show, Read, Generic, Typeable)



data CWE_Weakness = CWE_Weakness {
  weaknessAbstraction :: Text,
  weaknessStatus :: Text,
  weaknessID :: Text,
  weaknessName :: Text,
  weaknessDescription :: CWE_Description
} deriving (Eq, Show, Read, Generic, Typeable)



data CWE_Description = CWE_Description {
  descriptionSummary   :: Text,
  descriptionExtended  :: Maybe Text
} deriving (Eq, Show, Read, Generic, Typeable)



instance FromJSON CWE where
  parseJSON = withObject "weakness_catalog" $ \obj -> do
    case HM.lookup "Weakness_Catalog" obj of
      Just (Object o) -> do
        date <- o .: ("@Catalog_Date" :: Text)
        version <- o .: ("@Catalog_Version" :: Text)
        name    <- o .: ("@Catalog_Name" :: Text)
        categories <- case HM.lookup "Categories" o of
                        Just (Object o2) -> do
                          case HM.lookup "Category" o2 of
                            Just o3 -> parseJSON o3
                            _       -> fail "CWE:categories:cateogry"
                        _                -> fail "CWE:categories"
        weaknesses <- case HM.lookup "Weaknesses" o of
                        Just (Object o2) -> do
                          case HM.lookup "Weakness" o2 of
                            Just o3 -> parseJSON o3
                            _       -> fail "CWE:weaknesses:weakness"
                        _                -> fail "CWE:weakness"
        pure $ CWE {
          catalogDate = date,
          catalogVersion = version,
          catalogName = name,
          categories = categories,
          weaknesses = weaknesses
        }
      _ -> fail "CWE"



instance FromJSON CWE_Category where
  parseJSON = withObject "category" $ \o -> do
    status <- o .: "@Status"
    id     <- o .: "@ID"
    name   <- o .: "@Name"
    desc   <- o .: "Description"
    pure $ CWE_Category {
      categoryStatus = status,
      categoryID = id,
      categoryName = name,
      categoryDescription = desc
    }



instance FromJSON CWE_Weakness where
  parseJSON = withObject "weakness" $ \o -> do
    abstraction <- o .: "@Weakness_Abstraction"
    status      <- o .: "@Status"
    id          <- o .: "@ID"
    name        <- o .: "@Name"
    desc        <- o .: "Description"
    pure $ CWE_Weakness {
      weaknessAbstraction = abstraction,
      weaknessStatus = status,
      weaknessID = id,
      weaknessName = name,
      weaknessDescription = desc
    }



instance FromJSON CWE_Description where
  parseJSON = withObject "description" $ \o -> do
    summary <- case HM.lookup "Description_Summary" o of
                 Just (Object o2) -> o2 .: "#text" >>= pure . fixText
                 _                -> fail "CWE_Description:summary"
    extended <- case HM.lookup "Extended_Description" o of
                 Just (Object o2) -> case HM.lookup "Text" o2 of
                                       Just (Object o3) -> o3 .: "#text" >>= pure . Just . fixText
                                       _                -> pure Nothing
                 _                -> pure Nothing
    pure $ CWE_Description {
      descriptionSummary  = summary,
      descriptionExtended = extended
    }



fixText :: Text -> Text
fixText = replace "\n\t\t\t\t\t" " " . replace "\n\t\t\t\t\t" " "
