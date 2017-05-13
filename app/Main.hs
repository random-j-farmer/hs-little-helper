{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception           (throw)
import           Control.Logging             (debug, log, withStdoutLogging)
import           Control.Monad               (mapM_)
import           Data.Aeson                  (encode)
import qualified Data.ByteString.Lazy        as LB
import qualified Data.ByteString.Lazy.UTF8   as U
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (fromJust, fromMaybe)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import qualified Data.Text.Lazy              as LT
import           Eve.Api.Config
import           Eve.Api.Esi
import           Eve.Api.Http
import           Eve.Api.Interp
import           Eve.Api.Types
import           Eve.Api.Xml
import           Eve.Api.Zkill
import           Formatting                  (int, sformat, stext, (%))
import           Network.HTTP.Client.CertMan (setGlobalManagerFromPath)
import           Network.Socket              (withSocketsDo)
import           System.Environment          (getArgs, lookupEnv)


main :: IO ()
main = withSocketsDo $ withStdoutLogging $ do
  setGlobalManagerFromPath certificateStore
  args <- getArgs
  let filenames = if null args then [ "short.x" ] else args
  mapM_ handleFile filenames
  return ()

handleFile fn = do
  str <- TIO.readFile fn
  let names = characterName <$> T.lines str
  namesAndIds <- runHttpClientIO (getCharacterID names)
  mapM_ (mapM_ handleCharacter) namesAndIds

handleCharacter :: (CharacterName, CharacterID) -> IO ()
handleCharacter (name, charId) = do
  debug $ sformat ("id:" % int % "\tname: " % stext) (_characterID charId) (_characterName name)
  info <- runHttpClientIO $ getCharacterInfo charId
  let corpId = ciCorporationId $ fromRight info
  let allianceId = ciAllianceId $ fromRight info
  corp <- runHttpClientIO $ getCorporationInfo corpId
  alliance <- runHttpClientIO $ traverse getAllianceInfo allianceId
  stats <- runHttpClientIO $ getKillboardStats charId
  debug $ sformat ("char:" % stext % " corp:" % stext % " alliance:" % stext % " kills:" % int % " losses:" % int)
                  (ciName $ fromRight info)
                  (coCorporationName $ fromRight corp)
                  (fromMaybe "" $ aiAllianceName . fromRight <$> alliance)
                  (fromMaybe 0 $ ksshipsDestroyed $ fromRight stats)
                  (fromMaybe 0 $ ksshipsLost $ fromRight stats)

fromRight x = case x of
  Left y  -> throw (userError ("fromRight: Left " ++ show y))
  Right y -> y
