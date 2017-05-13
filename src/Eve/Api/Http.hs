{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
Module:      Eve.Api.Http
Description: Free HTTP Client
Copyright:   2017 Random J Farmer
License:     MIT
-}

module Eve.Api.Http
  ( HttpClientF(..)
  , HttpClient(..)
  , HttpClientResult(..)
  , HttpClientException(..)
  , httpGet
  , timedDebugMessage
  , guardSemaphore
  , getCharacterID
  , getCharacterInfo
  , getCorporationInfo
  , getAllianceInfo
  , getKillboardStats
  )
where

import           Control.Concurrent.MSem (MSem (..), new)
import           Control.Logging         (debug, timedDebug)
import           Control.Monad.Free
import           Control.Monad.Free.TH   (makeFree)
import           Data.Aeson              (FromJSON (..), eitherDecode, encode)
import qualified Data.ByteString.Lazy    as L
import           Data.Foldable           (fold)
import           Data.List.Split         (chunksOf)
import qualified Data.Map.Strict         as M
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Typeable           (Typeable)
import           Eve.Api.Config
import           Eve.Api.Esi
import           Eve.Api.Types
import           Eve.Api.Xml
import           Eve.Api.Zkill
import           Formatting              (int, sformat, stext, string, (%))
import           Network.HTTP.Client     (HttpException)
import           System.IO.Unsafe        (unsafePerformIO)
import           Text.Show.Functions


-- | DSL primitives
data HttpClientF next
  = HttpGet String (HttpClientResult L.ByteString -> next)
  -- test version must use different implementation - can't lookup chunnked names
  | GetCharacterIDChunk [CharacterName] (HttpClientResult [(CharacterName, CharacterID)] -> next)
  -- to allow logging from HttpClient monad
  | TimedDebugMessage Text next
  | DebugMessage Text next
  -- guard it with a semaphore
  | GuardSemaphore (MSem Int) next
  -- XXX; with additional type arguments, we are in a different monad :(
  | CachedID [CharacterName] (([(CharacterName, CharacterID)], [CharacterName]) -> next)
  | PutCachedID [(CharacterName, CharacterID)] next
  | CachedCharacter CharacterID (Maybe CharacterInfo -> next)
  | PutCachedCharacter CharacterID CharacterInfo next
  | CachedCorporation CorporationID (Maybe CorporationInfo -> next)
  | PutCachedCorporation CorporationID CorporationInfo next
  | CachedAlliance AllianceID (Maybe AllianceInfo -> next)
  | PutCachedAlliance AllianceID AllianceInfo next
  | CachedStats CharacterID (Maybe KillboardStats -> next)
  | PutCachedStats CharacterID KillboardStats next
  deriving (Show, Functor)

instance Show (MSem Int) where
  show _ = "MSem"

type HttpClientResult = Either HttpClientException

instance Monoid a => Monoid (HttpClientResult a) where
  mempty = Right   mempty
  mappend x y = case (x,y) of
    (Right x', Right y') -> Right $ x' `mappend` y'
    (Left x', _)         -> Left x'
    (_, Left y')         -> Left y'


-- | The exceptions we handle
data HttpClientException
  = HttpClientError HttpException
  | DecodeError String
  | OtherError String
  deriving (Show, Typeable)

makeFree ''HttpClientF

type HttpClient = Free HttpClientF


getCharacterInfo :: CharacterID -> HttpClient (HttpClientResult CharacterInfo)
getCharacterInfo k =
  getClientResult cachedCharacter putCachedCharacter esiApiSem
                  (sformat ("looking up character " % int) (_characterID k))
                  charInfoUrl
                  decodeClientResult
                  k

getCorporationInfo :: CorporationID -> HttpClient (HttpClientResult CorporationInfo)
getCorporationInfo k =
  getClientResult cachedCorporation putCachedCorporation esiApiSem
                (sformat ("looking up corporation " % int) (_corporationID k))
                corpInfoUrl
                decodeClientResult
                k

getAllianceInfo :: AllianceID -> HttpClient (HttpClientResult AllianceInfo)
getAllianceInfo k =
  getClientResult cachedAlliance putCachedAlliance esiApiSem
                (sformat ("looking up alliance " % int) (_allianceID k))
                allianceInfoUrl
                decodeClientResult
                k

getKillboardStats :: CharacterID -> HttpClient (HttpClientResult KillboardStats)
getKillboardStats k =
  getClientResult cachedStats putCachedStats zkillApiSem
                (sformat ("looking up killboard " % int) (_characterID k))
                zkillStatUrl
                decodeClientResult
                k

getClientResult getCache putCache sem msg toUrl decodeResult k = do
  cached <- getCache k
  case cached of
    Just x -> return $ Right x
    Nothing -> do
      guardSemaphore sem
      timedDebugMessage msg
      bytes <- httpGet (toUrl k)
      let result = decodeResult bytes
      mapM_ (putCache k) result
      return result


decodeClientResult :: FromJSON a => HttpClientResult L.ByteString -> HttpClientResult a
decodeClientResult result =
  case result of
    Left x -> Left x
    Right bs -> case eitherDecode bs of
      Left str -> Left $ DecodeError str
      Right a  -> Right a

-- | get the character ids for the named characters
getCharacterID :: [CharacterName] -> HttpClient (HttpClientResult [(CharacterName, CharacterID)])
getCharacterID names = do
  (known, unknown) <- cachedID names
  debugMessage $ sformat ("known/unknown: " % int % "/" % int) (length known) (length unknown)
  let chunks = chunksOf xmlApiChunkSize unknown
  result <- fold <$> traverse guardedCharIDChunk chunks
  mapM_ putCachedID result
  return $ (known ++) <$> result

guardedCharIDChunk names = do
  guardSemaphore xmlApiSem
  getCharacterIDChunk names

xmlApiSem :: MSem Int
{-# NOINLINE xmlApiSem #-}
xmlApiSem = unsafePerformIO $ do
  debug "Initalizing XML API Semaphore"
  new xmlApiConnections

esiApiSem :: MSem Int
{-# NOINLINE esiApiSem #-}
esiApiSem = unsafePerformIO $ do
  debug "Initalizing ESI Semaphore"
  new esiConnections

zkillApiSem :: MSem Int
{-# NOINLINE zkillApiSem #-}
zkillApiSem = unsafePerformIO $ do
  debug "Initalizing Zkill API Semaphore"
  new zkillConnections
