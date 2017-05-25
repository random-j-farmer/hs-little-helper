{-# LANGUAGE DeriveGeneric #-}
{- |
Module:      Eve.Api.Esi
Description: Eve Swagger Interface
Copyright:   (c) 2017 Random J Farmer
License:     MIT

Access to the various ESI Endpoints
-}

module Eve.Api.Esi
  ( charInfoUrl
  , corpInfoUrl
  , allianceInfoUrl
  , CharacterInfo(..)
  , CorporationInfo(..)
  , AllianceInfo(..)
  )
where

import           Control.DeepSeq             (NFData (..))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text)
import           Eve.Api.Config
import           Eve.Api.Types
import           GHC.Generics
import           Network.URL                 (add_param, exportURL, importURL)
import           Prelude

-- {"corporation_id": 98393635, "birthday": "2015-04-21T08:29:44Z", "name": "Random J Farmer", "gender": "male", "race_id":1, "bloodline_id": 2, "description": "", "alliance_id": 99007035, "ancestry_id": 7, "security_status": -1.1106748358672103}
{-
  {"corporation_id": 98393635, "birthday": "2015-04-21T08:29:44Z"
  , "name": "Random J Farmer", "gender": "male"
  , "race_id": 1, "bloodline_id": 2, "description": ""
  , "alliance_id": 99007035, "ancestry_id": 7
  , "security_status": -1.1106748358672103}
-}

data CharacterInfo = CharacterInfo
  { ciCorporationId  :: ! CorporationID
  , ciBirthday       :: ! Text
  , ciName           :: ! Text
  , ciGender         :: ! Text
  , ciRaceId         :: ! Integer
  , ciBloodlineId    :: ! Integer
  , ciDescription    :: ! Text
  , ciAllianceId     :: Maybe AllianceID
  , ciAncestryId     :: ! Integer
  , ciSecurityStatus :: ! Double
  } deriving (Show, Generic)

instance NFData CharacterInfo
instance ToJSON CharacterInfo where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
instance FromJSON CharacterInfo where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}

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
  { coCorporationName        :: ! Text
  , coTicker                 :: ! Text
  , coMemberCount            :: ! Int
  , coCeoID                  :: ! CharacterID
  , coCorporationDescription :: ! Text
  , coTaxRate                :: ! Double
  , coCreatorID              :: ! CharacterID
  , coUrl                    :: ! Text
  , coAllianceID             :: Maybe AllianceID
  , coCreationDate           :: Maybe Text
  , coFaction                :: Maybe Text
  } deriving (Show, Generic)

instance NFData CorporationInfo
instance ToJSON CorporationInfo where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
instance FromJSON CorporationInfo where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}


{-
{
  "alliance_name": "Black Shark Pantheon",
  "ticker": "JAWS-",
  "date_founded": "2016-12-07T15:33:51Z",
  "executor_corp": 98489187
}
-}
data AllianceInfo = AllianceInfo
  { aiAllianceName :: ! Text
  , aiTicker       :: ! Text
  , aiDateFounded  :: ! Text
  , aiExecutorCorp :: ! Integer
  } deriving (Show, Generic)

instance NFData AllianceInfo
instance ToJSON AllianceInfo where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
instance FromJSON AllianceInfo where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}

-- | Url for character lookup
charInfoUrl :: CharacterID -> String
charInfoUrl = esiUrl "characters"

-- | Url for corporation lookup
corpInfoUrl :: CorporationID -> String
corpInfoUrl = esiUrl "corporations"

-- | Url for alliance lookup
allianceInfoUrl :: AllianceID -> String
allianceInfoUrl = esiUrl "alliances"

esiUrl :: Show a => String -> a -> String
esiUrl service k =
  exportURL urlWithDatasource
  where
    base = "https://esi.tech.ccp.is/latest/" ++ service ++ "/"
    str = base ++ show k ++ "/"
    url = fromJust $ importURL str
    urlWithDatasource = add_param url ("datasource", esiDatasource)
