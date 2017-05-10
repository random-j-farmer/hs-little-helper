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
                     , CorporationID(..)
                     , CorporationInfo(..)
                     , AllianceID(..)
                     , AllianceInfo(..)
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


-- {"corporation_id": 98393635, "birthday": "2015-04-21T08:29:44Z", "name": "Random J Farmer", "gender": "male", "race_id":1, "bloodline_id": 2, "description": "", "alliance_id": 99007035, "ancestry_id": 7, "security_status": -1.1106748358672103}
{-
  {"corporation_id": 98393635, "birthday": "2015-04-21T08:29:44Z"
  , "name": "Random J Farmer", "gender": "male"
  , "race_id": 1, "bloodline_id": 2, "description": ""
  , "alliance_id": 99007035, "ancestry_id": 7
  , "security_status": -1.1106748358672103}
-}

data CharacterInfo = CharacterInfo
  { ciCorporationId  :: CorporationID
  , ciBirthday       :: Text
  , ciName           :: Text
  , ciGender         :: Text
  , ciRaceId         :: Integer
  , ciBloodlineId    :: Integer
  , ciDescription    :: Text
  , ciAllianceId     :: Maybe AllianceID
  , ciAncestryId     :: Integer
  , ciSecurityStatus :: Double
  } deriving (Show, Generic)

instance ToJSON CharacterInfo where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
instance FromJSON CharacterInfo where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}



-- | CorporationID is a numeric ID that identifies a corporation
newtype CorporationID = CorporationID { _corporationID :: Integer }
  deriving (Eq, Ord)
instance Show CorporationID where
  show ci = show (_corporationID ci)
instance ToJSON CorporationID where
  toJSON = Number . realToFrac . _corporationID
instance FromJSON CorporationID where
  parseJSON x = CorporationID <$> (parseJSON x :: Parser Integer)


{-
  {
    "corporation_name": "Black Shark Cult",
    "ticker": "-JAWS",
    "member_count": 114,
    "ceo_id": 93837174,
    "corporation_description": "<font size=\"12\" color=\"#bfffffff\">Black Shark Cult is a public community dedicated to practicing and teaching PvP in Eve Online. We are very newbie friendly but train to compete at a high level of skill.<br><br></font><font size=\"14\" color=\"#ffffffff\"><b>Black Shark Cult Mission<br><br></font><font size=\"12\" color=\"#bfffffff\">What:</b><br>1) Get newbies involved in group play as quickly as possible with the least barriers to entry.<br><br>2) Get newbies to experience PvP in which they play a meaningful part as quickly as possible.<br><br>3) Give newbies the opportunity to be quickly trained in high skill PvP<br><br>4) Give newbies the opportunity to quickly contribute meaningfully to the logistics (hauling / manufacturing) that supports a PvP group.<br><br>5) Become a feared, highly skilled, guerrilla warfare / special forces group that can punch above our weight and disrupt significant strategic objectives in New Eden.<br><br>6) Offer these special forces services to alliances and corporations as Mercenary Contracts.<br><br><b>How:</b><br>1) Be a shark - Have fun by flying around finding fights and practicing combat skills.<br>2) Connect with other sharks - Provide public small gang channel and Discord voice comms and chat server. New players can meet others and learn about PvP, anyone can announce a public roam, anyone can come and use our discord voice comms to run a gang, chat with friends or ask questions through out the day.<br><br>3) Preach the Gospel – Go to high sec hubs, and starter systems, to welcome people to the public channel and comms and fleets, and offer free fitted ships to new players. Older players are given fitted ships for a small fee.<br><br>4) Build Altars to Lord Jaws – Easy reshipping for newbies so they can get into the action quickly. Stock low sec hub stations in our main roaming areas with cheap low skill friendly, fitted t1 frigates / cruisers.<br><br>5) Initiate Lost Souls – Run regular public fleets into low sec + null sec, with pre fitted ships provided so newbies can get straight into PvP fleet action.<br><br>6) Establish Sects – PvP specialists establish training groups that teach the advanced principles of their discipline. IE advanced training camps for: Ewar, Tackling, Logi, Solo PvP, Small Gang, Fleet Commanding, Black Ops.<br><br>7) Make Disciples – Newly reborn Sharks join a sect to master its discipline, then they can become a teacher of that discipline or they can move on to learn another discipline.<br><br>8) Offer Mercenary services: Small Gang Harassment. EWAR wing for fleet fights. POS bash fleet. Blops Bomber gang. Entosis gangs.<br><br><br><br>Details<br>Join </font><font size=\"12\" color=\"#ffffa600\"><loc><a href=\"https://discord.gg/0nVpCp3osBSsyW1o\">Discord voice comms and chat server.</a></loc><br></font><font size=\"12\" color=\"#bfffffff\">Join \"Black Shark Cult\" chat channel in game.<br>Join \"Black Shark Cult\" mailing list<br>Black Shark Cult Reddit https://www.reddit.com/r/blacksharkcult</font>",
    "tax_rate": 0.07,
    "creator_id": 93837174,
    "url": "https://www.reddit.com/r/blacksharkcult/",
    "alliance_id": 99007035,
    "creation_date": "2015-04-27T09:30:43Z",
    "faction": "Caldari"
  }
-}
data CorporationInfo = CorporationInfo
  { coCorporationName        :: Text
  , coTicker                 :: Text
  , coMemberCount            :: Int
  , coCeoID                  :: CharacterID
  , coCorporationDescription :: Text
  , coTaxRate                :: Double
  , coCreatorID              :: CharacterID
  , coUrl                    :: Text
  , coAllianceID             :: Maybe AllianceID
  , coCreationDate           :: Maybe Text
  , coFaction                :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON CorporationInfo where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
instance FromJSON CorporationInfo where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}

-- | AllianceID is a numeric ID that identifies an alliance
newtype AllianceID = AllianceID { _allianceID :: Integer }
  deriving (Eq, Ord)
instance Show AllianceID where
  show ai = show (_allianceID ai)
instance ToJSON AllianceID where
  toJSON = Number . realToFrac . _allianceID
instance FromJSON AllianceID where
  parseJSON x = AllianceID <$> (parseJSON x :: Parser Integer)



{-
{
  "alliance_name": "Black Shark Pantheon",
  "ticker": "JAWS-",
  "date_founded": "2016-12-07T15:33:51Z",
  "executor_corp": 98489187
}
-}

data AllianceInfo = AllianceInfo
  { aiAllianceName :: Text
  , aiTicker       :: Text
  , aiDateFounded  :: Text
  , aiExecutorCorp :: Integer
  } deriving (Show, Generic)

instance ToJSON AllianceInfo where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
instance FromJSON AllianceInfo where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
