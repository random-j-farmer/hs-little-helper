{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy        as LB
import qualified Data.Map.Strict             as M
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import qualified Data.Text.Lazy              as LT


import           Control.Logging             (debug, log, withStdoutLogging)
import           Control.Monad               (mapM_)
import           Data.Maybe                  (fromJust)
import           Eve.Api.Config
import           Eve.Api.Esi
import           Eve.Api.Types
import           Eve.Api.Xml
import           Formatting                  (int, sformat, stext, (%))
import           Network.HTTP.Client.CertMan (setGlobalManagerFromPath)
import           Network.Socket              (withSocketsDo)
import           System.Environment          (getArgs, lookupEnv)


main :: IO ()
main = withSocketsDo $ withStdoutLogging $ do
  setGlobalManagerFromPath certificateStore
  args <- getArgs
  let filenames = if null args then [ "x.txt" ] else args
  mapM_ handleFile filenames
  return ()

handleFile fn = do
  str <- TIO.readFile fn
  let names = characterName <$> T.lines str
  byName <- lookupCharacterIDs names
  mapM_ (handleName byName) names

handleName :: M.Map CharacterName CharacterID -> CharacterName -> IO ()
handleName byName name = do
  debug $ sformat ("id:   " % int % "name: " % stext) cid cname
  info <- lookupCharacterInfo (fromJust (M.lookup name byName))
  print info
  where cid = maybe 0 _characterID (M.lookup name byName) :: Integer
        cname = _characterName name
