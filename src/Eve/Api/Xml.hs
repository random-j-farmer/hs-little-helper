{-# LANGUAGE OverloadedStrings #-}

{- |
Module:      Eve.Api.Xml
Description: XML API - ohly character id lookup
Copyright:   (c) 2017 Random J. Farmer
License:     MIT

-}
module Eve.Api.Xml (lookupCharacterIDs) where

import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as LB
import qualified Data.Map.Strict             as M
import qualified Data.Text                   as T

import           Control.Concurrent.MSem     (MSem, new)
import           Control.Exception           (Exception (..), throwIO)
import           Control.Logging             (debug, timedDebug)
import           Data.ByteString             (ByteString)
import           Data.Foldable               (foldl')
import           Data.List.Split             (chunksOf)
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text)
import           Data.Typeable               (Typeable (..))
import           Eve.Api.Config
import           Eve.Api.Par                 (parmap)
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

-- | Lookup the given character names and obtain their IDs
lookupCharacterIDs :: [CharacterName] -> IO (M.Map CharacterName CharacterID)
lookupCharacterIDs charNames = do
  sem <- apiSem
  M.unions <$> parmap sem _lookupCharacterIDs (chunksOf xmlApiChunkSize charNames)

apiSem :: IO (MSem Int)
apiSem = new xmlApiConnections

_lookupCharacterIDs :: [CharacterName] -> IO (M.Map CharacterName CharacterID)
_lookupCharacterIDs charNames =
  timedDebug (sformat ("lookupCharacterIDs: looking up " % int % " names")
                     (length charNames))
             (M.fromList . parseBody <$> (getURL . computeURL) charNames)


computeURL :: [CharacterName] -> String
computeURL charNames = urlWithNames where
    url = fromJust $ importURL "https://api.eveonline.com/eve/CharacterID.xml.aspx"
    tuples = (,) "names" . T.unpack . _characterName <$> charNames
    urlWithNames = exportURL $ foldr (flip add_param) url tuples

parseBody :: LB.ByteString -> [(CharacterName, CharacterID)]
parseBody = extractCharacterIDs . parseXml

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
