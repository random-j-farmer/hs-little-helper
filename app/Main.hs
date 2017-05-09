{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Logging             (debug, log, withStdoutLogging)
import           Control.Monad               (mapM_)
import qualified Data.ByteString.Lazy        as LB
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (fromJust)
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
  mapM_ (handleName byName) names

handleName :: M.Map CharacterName CharacterID -> CharacterName -> IO ()
handleName byName name = do
  debug $ sformat ("id:   " % int % "\tname: " % stext) cid cname
  info <- lookupCharacterInfo (fromJust (M.lookup name byName))
  corp <- lookupCorporationInfo (ciCorporationId info)
  alli <- lookupAllianceInfo (ciAllianceId info)
  debug $ sformat ("char:" % stext % " corp:" % stext) (ciName info) (coCorporationName corp)
  where cid = maybe 0 _characterID (M.lookup name byName) :: Integer
        cname = _characterName name
