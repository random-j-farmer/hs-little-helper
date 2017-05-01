{-# LANGUAGE DeriveGeneric     #-}
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
                     , CharacterInfo(..)
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
  deriving (Show, Eq, Ord)

-- | Constructor for a CharacterName
characterName :: Text -> CharacterName
characterName = MkCharacterName . T.toLower

-- | CharacterID is a numeric ID that identifies a Character
newtype CharacterID = CharacterID { _characterID :: Integer }
  deriving Show

-- {"corporation_id": 98393635, "birthday": "2015-04-21T08:29:44Z", "name": "Random J Farmer", "gender": "male", "race_id":1, "bloodline_id": 2, "description": "", "alliance_id": 99007035, "ancestry_id": 7, "security_status": -1.1106748358672103}
{-
  {"corporation_id": 98393635, "birthday": "2015-04-21T08:29:44Z"
  , "name": "Random J Farmer", "gender": "male"
  , "race_id": 1, "bloodline_id": 2, "description": ""
  , "alliance_id": 99007035, "ancestry_id": 7
  , "security_status": -1.1106748358672103}
-}

data CharacterInfo = CharacterInfo
  { ciCorporationId  :: Integer
  , ciBirthday       :: Text
  , ciName           :: Text
  , ciGender         :: Text
  , ciRaceId         :: Integer
  , ciBloodlineId    :: Integer
  , ciDescription    :: Text
  , ciAllianceId     :: Integer
  , ciAncestryId     :: Integer
  , ciSecurityStatus :: Double
  } deriving (Show, Generic)

instance ToJSON CharacterInfo where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
instance FromJSON CharacterInfo where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
