{-# LANGUAGE OverloadedStrings #-}
{- |
Module:      Eve.Api.Types
Description: Common Types for Eve Online APIs
Copyright:   (c) 2017 Random J. Farmer
LICENSE:     MIT

-}

module Eve.Api.Types ( CharacterName
                     , characterName
                     , _characterName
                     , CharacterID(..)
                     , CorporationID(..)
                     , AllianceID(..)
                     )
where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Ord
import           Data.Text        (Text)
import qualified Data.Text        as T
import           GHC.Generics

-- | CharacterName is a normalized form of a character's name.
--
-- For comparisons, it is always kept in lowercase
--
newtype CharacterName = MkCharacterName { _characterName :: Text }
  deriving (Eq, Ord)

instance Show CharacterName where
  show cn = show (_characterName cn)
instance FromJSON CharacterName where
  parseJSON x = characterName <$> (parseJSON x :: Parser Text)
instance ToJSON CharacterName where
  toJSON = String . _characterName
instance FromJSONKey CharacterName where
  fromJSONKey = FromJSONKeyText characterName
instance ToJSONKey CharacterName where
  toJSONKey = toJSONKeyText _characterName

-- | Constructor for a CharacterName
characterName :: Text -> CharacterName
characterName = MkCharacterName . T.toLower

-- | CharacterID is a numeric ID that identifies a Character
newtype CharacterID = CharacterID { _characterID :: Integer }
  deriving (Eq, Ord)

instance Show CharacterID where
  show ci = show (_characterID ci)
instance ToJSON CharacterID where
  toJSON = Number . realToFrac . _characterID
instance FromJSON CharacterID where
  parseJSON x = CharacterID <$> (parseJSON x :: Parser Integer)
instance FromJSONKey CharacterID where
  fromJSONKey = CharacterID <$> fromJSONKey
instance ToJSONKey CharacterID where
  toJSONKey = toJSONKeyText (T.pack . show)

-- | CorporationID is a numeric ID that identifies a corporation
newtype CorporationID = CorporationID { _corporationID :: Integer }
  deriving (Eq, Ord)
instance Show CorporationID where
  show ci = show (_corporationID ci)
instance ToJSON CorporationID where
  toJSON = Number . realToFrac . _corporationID
instance FromJSON CorporationID where
  parseJSON x = CorporationID <$> (parseJSON x :: Parser Integer)
instance FromJSONKey CorporationID where
  fromJSONKey = CorporationID <$> fromJSONKey
instance ToJSONKey CorporationID where
  toJSONKey = toJSONKeyText (T.pack . show)


-- | AllianceID is a numeric ID that identifies an alliance
newtype AllianceID = AllianceID { _allianceID :: Integer }
  deriving (Eq, Ord)
instance Show AllianceID where
  show ai = show (_allianceID ai)
instance ToJSON AllianceID where
  toJSON = Number . realToFrac . _allianceID
instance FromJSON AllianceID where
  parseJSON x = AllianceID <$> (parseJSON x :: Parser Integer)
instance FromJSONKey AllianceID where
  fromJSONKey = AllianceID <$> fromJSONKey
instance ToJSONKey AllianceID where
  toJSONKey = toJSONKeyText (T.pack . show)
