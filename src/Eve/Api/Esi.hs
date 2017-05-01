{-# LANGUAGE OverloadedStrings #-}

{- |
Module:      Eve.Api.Esi
Description: Eve Swagger Interface
Copyright:   (c) 2017 Random J Farmer
License:     MIT

Access to the various ESI Endpoints
-}

module Eve.Api.Esi ( lookupCharacterInfo
                   )
where

import qualified Data.ByteString.Lazy        as LB

import Data.Aeson
import           Control.Concurrent.MSem     (MSem, new)
import           Control.Logging             (timedDebug)
import           Data.Maybe                  (fromJust)
import           Eve.Api.Config
import           Eve.Api.Types
import           Formatting                  (sformat, int, (%))
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
    (parseBody <$>getURL (charInfoUrl charId))


charInfoUrl :: CharacterID -> String
charInfoUrl charId =
  exportURL urlWithDatasource
  where
    base = "https://esi.tech.ccp.is/latest/characters/"
    str = base ++ show (_characterID charId) ++ "/"
    url = fromJust $ importURL str
    urlWithDatasource = add_param url ("datasource", esiDatasource)

parseBody :: LB.ByteString -> CharacterInfo
parseBody = fromJust . decode
