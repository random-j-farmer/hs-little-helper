{-# LANGUAGE OverloadedStrings #-}
module Main where

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
import           Eve.Api.Cache
import           Eve.Api.Config
import           Eve.Api.Types
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
  byName <- M.fromList <$> lookupCharacterIDs names
  mapM_ (handleName byName) $ M.keys byName

handleName :: M.Map CharacterName CharacterID -> CharacterName -> IO ()
handleName byName name = do
  debug $ sformat ("id:" % int % "\tname: " % stext) (_characterID cid) cname
  let charId = fromJust $ M.lookup name byName
  info <- lookupCharacterInfo charId
  corp <- lookupCorporationInfo $ ciCorporationId info
  alliance <- sequence $ lookupAllianceInfo <$> ciAllianceId info
  stats <- lookupKillboardStats charId
  debug $ sformat ("char:" % stext % " corp:" % stext % " alliance:" % stext % " kills:" % int % " losses:" % int)
                  (ciName info)
                  (coCorporationName corp)
                  (fromMaybe "" $ aiAllianceName <$> alliance)
                  (fromMaybe 0 $ ksshipsDestroyed stats)
                  (fromMaybe 0 $ ksshipsLost stats)
  -- let statsStr = Data.Aeson.encode stats
  -- debug $ T.pack $ U.toString statsStr
  where cid = fromJust $ M.lookup name byName
        cname = _characterName name
