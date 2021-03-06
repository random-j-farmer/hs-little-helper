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

import           Control.Exception    (throw)
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe           (fromJust)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Eve.Api.Types
import           Network.URL          (add_param, exportURL, importURL)
import           Prelude
import           Text.XML.Expat.Proc  (findChild, findChildren)
import           Text.XML.Expat.Tree  (UNode, XMLParseError,
                                       defaultParseOptions, getAttribute, parse)

characterIDUrl :: [CharacterName] -> String
characterIDUrl charNames = urlWithNames where
    url = fromJust $ importURL "https://api.eveonline.com/eve/CharacterID.xml.aspx"
    tuples = (,) "names" . T.unpack . _characterName <$> charNames
    urlWithNames = exportURL $ foldr (flip add_param) url tuples

parseXMLBody :: LB.ByteString -> [(CharacterName, CharacterID)]
parseXMLBody body =
  case parseXml body of
    (_, Just x) -> throw $ HttpClientXmlParseError x
    (x, _)      -> extractCharacterIDs x

parseXml :: LB.ByteString -> (UNode Text, Maybe XMLParseError)
parseXml = parse defaultParseOptions

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
  filter (\x -> _characterID (snd x) /= 0)
         (row2pair <$> rows)
  where rows = (findChildren "row" . child "rowset" . child "result") rootNode
        row2pair node = ( characterName $ attr "name" node
                        , CharacterID . read $ T.unpack $ attr "characterID" node)

-- | extract single named child or error
child :: Text -> UNode Text -> UNode Text
child tag = fromJust . findChild tag

-- | extract named attribute
attr :: Text -> UNode Text -> Text
attr tag node = fromJust $ getAttribute node tag
