{-# LANGUAGE OverloadedStrings #-}

{- |
Module:      Eve.Api.Esi
Description: Eve Swagger Interface
Copyright:   (c) 2017 Random J Farmer
License:     MIT

Access to the various ESI Endpoints
-}

module Eve.Api.Esi ( lookupCharacterInfo
                   , lookupCorporationInfo
                   , lookupAllianceInfo
                   )
where

import qualified Data.ByteString.Lazy        as LB

import           Control.Concurrent.MSem     (MSem, new)
import           Control.Logging             (timedDebug)
import           Data.Aeson
import           Data.Maybe                  (fromJust)
import           Eve.Api.Config
import           Eve.Api.Types
import           Formatting                  (int, sformat, (%))
import           Network.HTTP.Client.CertMan (getURL)
import           Network.URL                 (add_param, exportURL, importURL)

esiSem :: IO(MSem Int)
esiSem = new esiConnections

-- | Public character info
--
-- https://esi.tech.ccp.is/latest/characters/666/?datasource=tranquility
lookupCharacterInfo :: CharacterID -> IO CharacterInfo
lookupCharacterInfo charId =
  timedDebug (sformat ("looking up character " % int) (_characterID charId))
    ((fromJust . decode) <$> getURL (charInfoUrl charId))

charInfoUrl :: CharacterID -> String
charInfoUrl = esiUrl "characters"

corpInfoUrl :: CorporationID -> String
corpInfoUrl = esiUrl "corporations"

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

-- | Public Corporation info
--
-- https://esi.tech.ccp.is/latest/corporations/666/?datasource=tranquility
lookupCorporationInfo :: CorporationID -> IO CorporationInfo
lookupCorporationInfo corpId =
  timedDebug (sformat ("looking up corporation " % int) (_corporationID corpId))
    ((fromJust . decode) <$> getURL (corpInfoUrl corpId))

-- | Public Alliance info
--
-- https://esi.tech.ccp.is/latest/alliances/666/?datasource=tranquility
lookupAllianceInfo :: AllianceID -> IO AllianceInfo
lookupAllianceInfo aid =
  timedDebug (sformat ("looking up alliance " % int) (_allianceID aid))
    ((fromJust . decode) <$> getURL (allianceInfoUrl aid))
