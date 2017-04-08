{-# LANGUAGE OverloadedStrings #-}

module LN.Api.Ingest.CWE.Types (
  CWE (..),
  CWE_Category (..),
  CWE_Weakness (..),
  CWE_Description (..),
  CWE_Platform (..)
) where



import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Text
import           Data.Typeable
import           GHC.Generics



data CWE = CWE {
  catalogDate    :: Text,
  catalogVersion :: Text,
  catalogName    :: Text,
  categories     :: [CWE_Category],
  weaknesses     :: [CWE_Weakness]
} deriving (Eq, Show, Read, Generic, Typeable)



data CWE_Category = CWE_Category {
  categoryStatus      :: Text,
  categoryID          :: Text,
  categoryName        :: Text,
  categoryDescription :: CWE_Description
  -- categoryApplicationPlatform
  -- categoryTimeOfIntroduction
} deriving (Eq, Show, Read, Generic, Typeable)



data CWE_Weakness = CWE_Weakness {
  weaknessAbstraction        :: Text,
  weaknessStatus             :: Text,
  weaknessID                 :: Text,
  weaknessName               :: Text,
  weaknessDescription        :: CWE_Description,
  weaknessPlatforms          :: [CWE_Platform],
  weaknessTimeOfIntroduction :: [CWE_TimeOfIntroduction]
} deriving (Eq, Show, Read, Generic, Typeable)



data CWE_Description = CWE_Description {
  descriptionSummary  :: Text,
  descriptionExtended :: Maybe Text
} deriving (Eq, Show, Read, Generic, Typeable)



data CWE_Platform = CWE_Platform {
  platformName :: Text
} deriving (Eq, Show, Read, Generic, Typeable)



data CWE_TimeOfIntroduction = CWE_TimeOfIntroduction {
  timeOfIntroduction :: Text
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
    platforms   <- case HM.lookup "Applicable_Platforms" o of
                        Just (Object o2) -> do
                          case HM.lookup "Languages" o2 of
                               Just (Object o3) -> case (HM.lookup "Language" o3, HM.lookup "Language_Class" o3) of
                                                        (Just o4@(Object _), _)  -> do
                                                          p <- parseJSON o4
                                                          pure [p]
                                                        (Just o4@(Array _), _) -> parseJSON o4
                                                        (Nothing, Just o5@(Object _)) -> do
                                                                    p <- parseJSON o5
                                                                    pure [p]
                                                        (Nothing, Just o5@(Array _)) -> parseJSON o5
                                                        _       -> fail "CWE:weakness:languages:language_class"
                               _ -> pure []
                        _                -> pure []
    time_of_intro <- case HM.lookup "Time_of_Introduction" o of
                          Just (Object o2) -> do
                            case HM.lookup "Introductory_Phase" o2 of
                                 Just o4@(Object _)  -> do
                                   p <- parseJSON o4
                                   pure [p]
                                 Just o4@(Array _) -> parseJSON o4
                                 _       -> fail "CWE:weakness:time_of_inroduction:language_class"
                          _                -> pure []
    pure $ CWE_Weakness {
      weaknessAbstraction = abstraction,
      weaknessStatus = status,
      weaknessID = id,
      weaknessName = name,
      weaknessDescription = desc,
      weaknessPlatforms = platforms,
      weaknessTimeOfIntroduction = time_of_intro
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



instance FromJSON CWE_Platform where
  parseJSON = withObject "platform" $ \o -> do
    m_name <- o .:? "@Language_Name"
    m_class <- o .:? "@Language_Class_Description"
    case (m_name, m_class) of
      (Just name, _) ->
        pure $ CWE_Platform {
          platformName = name
        }
      (Nothing, Just class_) ->
        pure $ CWE_Platform {
          platformName = class_
        }
      _ -> pure $ CWE_Platform "unknown"



instance FromJSON CWE_TimeOfIntroduction where
  parseJSON = withObject "time_of_introduction" $ \o -> do
    name <- o .: "#text"
    pure $ CWE_TimeOfIntroduction {
      timeOfIntroduction = name
    }



fixText :: Text -> Text
fixText = replace "\t" "" . replace "\n\t\t\t\t\t" " " . replace "\n\t\t\t\t\t" " "
