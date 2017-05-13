{-# LANGUAGE OverloadedStrings #-}

{- |
Module:      Eve.Api.Xml
Description: XML API - ohly character id lookup
Copyright:   (c) 2017 Random J. Farmer
License:     MIT

-}
module Eve.Api.Xml
  ( characterIDUrl
  , parseXMLBody)
  where

import           Control.Concurrent.MSem     (MSem, new)
import           Control.Exception           (Exception (..), throwIO)
import           Control.Logging             (debug, timedDebug)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as LB
import           Data.Foldable               (foldl')
import           Data.List.Split             (chunksOf)
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Typeable               (Typeable (..))
import           Eve.Api.Config
import           Eve.Api.Types
import           Formatting                  (int, sformat, shown, text, (%))
import           Network.HTTP.Client
import           Network.HTTP.Client.CertMan (getURL)
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status   (statusCode)
import           Network.URL                 (add_param, exportURL, importURL)
import           System.Environment          (lookupEnv)
import           Text.XML.Expat.Proc         (findChild, findChildren)
import           Text.XML.Expat.Tree         (UNode, XMLParseError,
                                              defaultParseOptions, getAttribute,
                                              parseThrowing)

characterIDUrl :: [CharacterName] -> String
characterIDUrl charNames = urlWithNames where
    url = fromJust $ importURL "https://api.eveonline.com/eve/CharacterID.xml.aspx"
    tuples = (,) "names" . T.unpack . _characterName <$> charNames
    urlWithNames = exportURL $ foldr (flip add_param) url tuples

-- XXX: return a either, don't use parseThrowing
parseXMLBody :: LB.ByteString -> [(CharacterName, CharacterID)]
parseXMLBody = filter (\x -> _characterID (snd x) /= 0) . extractCharacterIDs . parseXml

parseXml :: LB.ByteString -> UNode Text
parseXml = parseThrowing defaultParseOptions

{-
<?xml version='1.0' encoding='UTF-8'?>
<eveapi version="2">
  <currentTime>2017-04-22 18:10:51</currentTime>
  <result>
    <rowset name="characters" key="characterID" columns="name,characterID">
      <row name="Random J Farmer" characterID="95538430" />
    </rowset>
  </result>
  <cachedUntil>2017-05-22 18:10:51</cachedUntil>
</eveapi>
-}
extractCharacterIDs :: UNode Text -> [(CharacterName, CharacterID)]
extractCharacterIDs rootNode =
  row2pair <$> rows
  where rows = (findChildren "row" . child "rowset" . child "result") rootNode
        row2pair node = ( characterName $ attr "name" node
                        , CharacterID . read $ T.unpack $ attr "characterID" node)

-- | extract single named child or error
child :: Text -> UNode Text -> UNode Text
child tag = fromJust . findChild tag

-- | extract named attribute
attr :: Text -> UNode Text -> Text
attr tag node = fromJust $ getAttribute node tag
