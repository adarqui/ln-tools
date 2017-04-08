{-# LANGUAGE OverloadedStrings #-}

module LN.Api.Ingest.CWE.Types (
  CWE (..),
  CWE_Category (..),
  CWE_Weakness (..),
  CWE_Description (..),
  CWE_Platform (..),
  CWE_Consequence (..),
  CWE_TimeOfIntroduction (..),
  CWE_BangTextField (..),
  CWE_TextField (..),
  CWE_TermField (..)
) where



import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Monoid
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
  weaknessTimeOfIntroduction :: [CWE_TimeOfIntroduction],
  weaknessConsequences       :: Maybe CWE_Consequence,
  weaknessAlternateTerms     :: [CWE_TermField],
  weaknessTerminologyNotes   :: [CWE_TextField]
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



data CWE_Consequence = CWE_Consequence {
  consequenceScope :: [CWE_BangTextField],
  consequenceTechnicalImpact :: [CWE_BangTextField],
  consequenceNotes :: [CWE_BangTextField]
} deriving (Eq, Show, Read, Generic, Typeable)



data CWE_BangTextField = CWE_BangTextField {
  bangText :: Maybe Text
} deriving (Eq, Show, Read, Generic, Typeable)



data CWE_TextField = CWE_TextField {
  text :: Text
} deriving (Eq, Show, Read, Generic, Typeable)



data CWE_TermField = CWE_TermField {
  term :: Text
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

    consequences <- case HM.lookup "Common_Consequences" o of
                          Just (Object o2) -> do
                            case HM.lookup "Common_Consequence" o2 of
                                 Just o4@(Object _)  -> parseJSON o4 >>= pure . Just
                                 _ -> pure Nothing
                          _                -> pure Nothing

    alternate <- case HM.lookup "Alternate_Terms" o of
                      Just (Object o2) -> do
                        case HM.lookup "Alternate_Term" o2 of
                             Just (Object o3) ->
                               case HM.lookup "Term" o3 of
                                 Just o4@(Object _) -> do
                                   p <- parseJSON o4
                                   pure [p]
                                 Just o4@(Array _) -> parseJSON o4
                                 _ -> pure []
                             _ -> pure []
                      _ -> pure []

    termnotes <- case HM.lookup "Terminology_Notes" o of
                      Just (Object o2) -> do
                        case HM.lookup "Terminology_Note" o2 of
                             Just (Object o3) ->
                               case HM.lookup "Text" o3 of
                                 Just o4@(Object _) -> do
                                   p <- parseJSON o4
                                   pure [p]
                                 Just o4@(Array _) -> parseJSON o4
                                 _ -> pure []
                             _ -> pure []
                      _ -> pure []

    pure $ CWE_Weakness {
      weaknessAbstraction = abstraction,
      weaknessStatus = status,
      weaknessID = id,
      weaknessName = name,
      weaknessDescription = desc,
      weaknessPlatforms = platforms,
      weaknessTimeOfIntroduction = time_of_intro,
      weaknessConsequences = consequences,
      weaknessAlternateTerms = alternate,
      weaknessTerminologyNotes = termnotes
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




instance FromJSON CWE_Consequence where
  parseJSON = withObject "consequence" $ \o -> do
    scope <- case HM.lookup "Consequence_Scope" o of
                  Just o4@(Object _)  -> do
                    p <- parseJSON o4
                    pure [p]
                  Just o4@(Array _) -> parseJSON o4
                  _       -> pure [] -- fail "CWE:consequence:scope"
    impact <- case HM.lookup "Consequence_Technical_Impact" o of
                  Just o4@(Object _)  -> do
                    p <- parseJSON o4
                    pure [p]
                  Just o4@(Array _) -> parseJSON o4
                  _       -> pure [] -- fail "CWE:consequence:technical_impact"
    notes <- case HM.lookup "Consequence_Note" o of
                  Just (Object o2) -> case HM.lookup "Text" o2 of
                                           Just o4@(Object _) -> do
                                             p <- parseJSON o4
                                             pure [p]
                                           Just o4@(Array _) -> parseJSON o4
                                           _ -> pure [] -- fail "CWE:consequence:note:Text"
                  _ -> pure [] -- fail "CWE:consequence:note"
    pure $ CWE_Consequence {
      consequenceScope = scope,
      consequenceTechnicalImpact = impact,
      consequenceNotes = notes
    }



instance FromJSON CWE_BangTextField where
  parseJSON = withObject "bang_text_field" $ \o -> do
    m_text <- o .:? "#text" >>= pure . (<$>) fixText
    pure $ CWE_BangTextField m_text



instance FromJSON CWE_TextField where
  parseJSON = withObject "text_field" $ \o -> do
    text <- o .: "#text" >>= pure . fixText
    pure $ CWE_TextField text



instance FromJSON CWE_TermField where
  parseJSON = withObject "term_field" $ \o -> do
    term <- o .: "#text" >>= pure . fixText
    pure $ CWE_TermField term



fixText :: Text -> Text
fixText = replace "\t" "" . replace "\n\t\t\t\t\t" " " . replace "\n\t\t\t\t\t" " "
