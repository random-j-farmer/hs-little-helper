{-# LANGUAGE DeriveGeneric #-}
{- |
Module:      Eve.Api.Zkill
Description: ZKillboard API (stats)
Copyright:   (c) 2017 Random J. Farmer
License:     MIT

Not all info is parsed (groups, newtype wrapper type strings).

-}
module Eve.Api.Zkill
  ( zkillStatUrl
  , KillboardStats(..)
  , KillboardMonth(..)
  , ActivePvp(..)
  , ActivePvpShips(..)
  , ActivePvpSystems(..)
  , ActivePvpKills(..)
  , ZkillInfo(..)
  ) where

import           Control.DeepSeq             (NFData (..))
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text)
import           Eve.Api.Types
import           GHC.Generics                (Generic)
import           Network.URL                 (exportURL, importURL)
import           Prelude

-- | ZKillboard Killboard Statistics for a Pilot
data KillboardStats =
  KillboardStats
    { ksid             :: Maybe CharacterID
    , ksiskDestroyed   :: Maybe Double
    , ksiskLost        :: Maybe Double
    , ksshipsDestroyed :: Maybe Int
    , ksshipsLost      :: Maybe Int
    , kssoloKills      :: Maybe Int
    , kssoloLosses     :: Maybe Int
    , ksactivepvp      :: Maybe ActivePvp
    , ksmonths         :: Maybe (M.Map Text KillboardMonth)
    , ksinfo           :: Maybe ZkillInfo
    } deriving (Show, Generic)
instance NFData KillboardStats
instance ToJSON KillboardStats where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 2}
instance FromJSON KillboardStats where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

-- | Active Pvp info - number of kills/ships/systems in last 7 days
data ActivePvp =
  ActivePvp
    { apships   :: Maybe ActivePvpShips
    , apsystems :: Maybe ActivePvpSystems
    , apkills   :: Maybe ActivePvpKills
    } deriving (Show, Generic)
instance NFData ActivePvp
instance ToJSON ActivePvp where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 2}
instance FromJSON ActivePvp where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}


newtype ActivePvpShips = ActivePvpShips { apshipcount :: Int } deriving (Show, Generic)
instance NFData ActivePvpShips
instance ToJSON ActivePvpShips where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 6}
instance FromJSON ActivePvpShips where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 6}

newtype ActivePvpSystems = ActivePvpSystems { apsystemcount :: Int } deriving (Show, Generic)
instance NFData ActivePvpSystems
instance ToJSON ActivePvpSystems where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 8}
instance FromJSON ActivePvpSystems where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 8}

newtype ActivePvpKills = ActivePvpKills { apkillcount :: Int } deriving (Show, Generic)
instance NFData ActivePvpKills
instance ToJSON ActivePvpKills where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 6}
instance FromJSON ActivePvpKills where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 6}

data KillboardMonth =
  KillboardMonth
    { kmyear           :: ! Int
    , kmmonth          :: ! Int
    , kmshipsDestroyed :: Maybe Int
    , kmshipsLost      :: Maybe Int
    , kmiskDestroyed   :: Maybe Double
    , kmiskLost        :: Maybe Double
    } deriving (Show, Generic)
instance NFData KillboardMonth
instance ToJSON KillboardMonth where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 2}
instance FromJSON KillboardMonth where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}


data ZkillInfo =
  ZkillInfo
    { ziallianceID    :: Maybe AllianceID
    , zicorporationID :: ! CorporationID
    , zifactionID     :: Maybe Int
    , zikillID        :: Maybe Integer
    , ziname          :: ! Text
    , zisecStatus     :: ! Double
    } deriving (Show, Generic)
instance NFData ZkillInfo
instance ToJSON ZkillInfo where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 2}
instance FromJSON ZkillInfo where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

-- https://zkillboard.com/api/stats/characterID/1192491827/
zkillStatUrl :: CharacterID -> String
zkillStatUrl charId =
  exportURL url
  where
    str = "https://zkillboard.com/api/stats/characterID/" ++ show charId ++ "/"
    url = fromJust $ importURL str
