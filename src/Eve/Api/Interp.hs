{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
Module:      Eve.Api.Interp
Description: Interpreters for the free Http Client
Copyright:   2017 Random J Farmer
License:     MIT
-}

module Eve.Api.Interp
  ( runHttpClientIO
  , runTestIO
  , loadIOCaches
  )
where

import           Control.Concurrent.MSem     (MSem (..), new, signal, wait)
import           Control.Exception           (bracket_, catch, throw)
import           Control.Logging             (debug, timedDebug)
import           Control.Monad.Free
import           Data.Aeson                  (FromJSON (..), eitherDecode,
                                              encode)
import qualified Data.ByteString.Lazy        as L
import           Data.Foldable               (fold)
import qualified Data.Map.Strict             as M
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Eve.Api.Cache               (Cached (..))
import           Eve.Api.Config
import           Eve.Api.Esi
import           Eve.Api.Http
import           Eve.Api.Types
import           Eve.Api.Xml
import           Eve.Api.Zkill
import           Formatting                  (int, sformat, stext, string, (%))
import           Network.HTTP.Client         (HttpException)
import           Network.HTTP.Client.CertMan (getURL)

-- | interprets the http client monad, performing real io
runHttpClientIO :: HttpClient a -> IO a
runHttpClientIO = iterM run where
  run :: HttpClientF (IO a) -> IO a
  run (HttpGet url f)               = getURLEither url >>= f
  run (GetCharacterIDChunk names f) = getCharIDChunkIO names >>= f
  run (TimedDebugMessage msg f)     = timedDebug msg f
  run (GuardSemaphore sem f)        = guardIO sem f


guardIO sem =
  bracket_
    (wait sem >> debug "acquired semaphore")
    (debug "releasing semaphore" >> signal sem)

getCharIDChunkIO names =
  timedDebug (sformat ("looking up character ids: " % int % " names")
                      (length names)) $ do
    let url = characterIDUrl names
    result <- runHttpClientIO $ httpGet url
    return $ case result of
      Left x   -> Left x
      Right bs -> Right $ parseXMLBody bs

getURLEither :: String -> IO (HttpClientResult L.ByteString)
getURLEither url = do
    bs <- getURL url
    return (Right bs)
  `catch`
    \ex -> (return . Left . HttpClientError) ex

-- | interprets the http client monad against a map of url and content
runTestIO :: M.Map String L.ByteString -> HttpClient a -> IO a
runTestIO ioCache = iterM run where
  run :: HttpClientF (IO a) -> IO a
  run (HttpGet url f)               = fakeURLEither ioCache url >>= f
  run (TimedDebugMessage msg f)     = timedDebug msg f
  run (GuardSemaphore sem f)        = guardIO sem f
  run (GetCharacterIDChunk names f) = do
    results <- traverse (\x -> getCharIDChunkIO [x]) names
    f $ fold results

fakeURLEither :: M.Map String L.ByteString -> String -> IO (HttpClientResult L.ByteString)
fakeURLEither ioCache url =
    return $ case M.lookup url ioCache of
      Nothing -> Left $ OtherError $ "not cached: " ++ url
      Just x  -> Right x

-- | Loads IO Cache for runTestIO
loadIOCaches :: IO (M.Map String L.ByteString)
loadIOCaches = do
  ids <- (fromRight . eitherDecode) <$> L.readFile "dump/id_by_name.json" :: IO (M.Map CharacterName CharacterID)
  infos <- (fromRight . eitherDecode) <$> L.readFile "dump/characters.json" :: IO (M.Map CharacterID (Cached CharacterInfo))
  corps <- (fromRight . eitherDecode) <$> L.readFile "dump/corporations.json" :: IO (M.Map CorporationID (Cached CorporationInfo))
  alliances <- (fromRight . eitherDecode) <$> L.readFile "dump/alliances.json" :: IO (M.Map AllianceID (Cached AllianceInfo))
  killboard <- (fromRight . eitherDecode) <$> L.readFile "dump/killboards.json" :: IO (M.Map CharacterID (Cached Eve.Api.Zkill.KillboardStats))
  return $ Prelude.foldr M.union M.empty
    [ enc (\x -> characterIDUrl [x]) ids
    , enc charInfoUrl infos
    , enc corpInfoUrl corps
    , enc allianceInfoUrl alliances
    , enc zkillStatUrl killboard
    ]
  where
    enc f = M.map encode . M.mapKeys f

loadIOCache toUrl toBS fn = (M.map toBS . M.mapKeys toUrl . fromRight . eitherDecode) <$> L.readFile fn

fromRight x = case x of
  Left y  -> throw (userError ("fromRight: Left " ++ show y))
  Right y -> y
