{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
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
  , ApiCombinator(..)
  , httpGet
  , xmlApiSem
  , getCharacterID
  , getCharacterInfo
  , getCorporationInfo
  , getAllianceInfo
  , getKillboardStats
  , combinedLookup
  )
where

import           Control.Concurrent.MSem (MSem (..), new)
import           Control.DeepSeq         (NFData(..))
import           Control.Logging         (debug, timedDebug)
import           Control.Monad           (join)
import           Control.Monad.Free
import           Control.Monad.Free.TH   (makeFree)
import           Data.Aeson              (FromJSON (..), eitherDecode, encode)
import qualified Data.ByteString.Lazy    as L
import           Data.Foldable           (fold)
import           Data.List.Split         (chunksOf)
import qualified Data.Map.Strict         as M
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Eve.Api.Config
import           Eve.Api.Esi
import           Eve.Api.Types
import           Eve.Api.Xml
import           Eve.Api.Zkill
import           Formatting              (int, sformat, stext, string, (%))
import           GHC.Generics
import           System.IO.Unsafe        (unsafePerformIO)
import           Text.Show.Functions

-- | DSL primitives
data HttpClientF next
  = HttpGet (MSem Int) String (HttpClientResult L.ByteString -> next)
  -- test version must use different implementation - can't lookup chunnked names
  | GetCharacterIDChunk [CharacterName] (HttpClientResult [(CharacterName, CharacterID)] -> next)
  -- to allow logging from HttpClient monad
  | DebugMessage Text next
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
  deriving (Show, Functor, Generic)

instance Show (MSem Int) where
  show _ = "MSem"
instance NFData (MSem Int) where
  rnf _ = ()
instance NFData a => NFData (HttpClientF a)

makeFree ''HttpClientF

type HttpClient = Free HttpClientF


getCharacterInfo :: CharacterID -> HttpClient (HttpClientResult CharacterInfo)
getCharacterInfo k =
  getClientResult cachedCharacter putCachedCharacter esiApiSem
                  (sformat ("looking up character " % int) (_characterID k))
                  charInfoUrl
                  (decodeClientResult (show k))
                  k

getCorporationInfo :: CorporationID -> HttpClient (HttpClientResult CorporationInfo)
getCorporationInfo k =
  getClientResult cachedCorporation putCachedCorporation esiApiSem
                (sformat ("looking up corporation " % int) (_corporationID k))
                corpInfoUrl
                (decodeClientResult (show k))
                k

getAllianceInfo :: Maybe AllianceID -> HttpClient (HttpClientResult (Maybe AllianceInfo))
getAllianceInfo Nothing = return $ Right Nothing
getAllianceInfo (Just k) = do
  let result = getClientResult cachedAlliance putCachedAlliance esiApiSem
                (sformat ("looking up alliance " % int) (_allianceID k))
                allianceInfoUrl
                (decodeClientResult (show k))
                k
  fmap (fmap Just) result

getKillboardStats :: CharacterID -> HttpClient (HttpClientResult KillboardStats)
getKillboardStats k =
  getClientResult cachedStats putCachedStats zkillApiSem
                (sformat ("looking up killboard " % int) (_characterID k))
                zkillStatUrl
                (decodeClientResult (show k))
                k

getClientResult getCache putCache sem msg toUrl decodeResult k = do
  cached <- getCache k
  case cached of
    Just x -> return $ Right x
    Nothing -> do
      debugMessage msg
      bytes <- httpGet sem (toUrl k)
      let result = decodeResult bytes
      mapM_ (putCache k) result
      return result


-- | Helper to decode a client result
decodeClientResult :: FromJSON a => String -> HttpClientResult L.ByteString -> HttpClientResult a
decodeClientResult errCtx result =
  case result of
    Left x -> Left x
    Right bs -> case eitherDecode bs of
      Left str -> Left $ DecodeError (errCtx ++ ": " ++ str)
      Right a  -> Right a

-- | get the character ids for the named characters
getCharacterID :: [CharacterName] -> HttpClient (HttpClientResult [(CharacterName, CharacterID)])
getCharacterID names = do
  (known, unknown) <- cachedID names
  debugMessage $ sformat ("known/unknown: " % int % "/" % int) (length known) (length unknown)
  let chunks = chunksOf xmlApiChunkSize unknown
  result <- fold <$> traverse getCharacterIDChunk chunks
  mapM_ putCachedID result
  return $ (known ++) <$> result

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

bindHttpClient :: (a -> HttpClient (HttpClientResult b)) ->  HttpClientResult a -> HttpClient (HttpClientResult b)
bindHttpClient bindFn result =
  case result of
    Right x -> bindFn x
    Left x  -> return $ Left x

type ApiCombinator a = CharacterID -> CharacterInfo -> CorporationInfo -> Maybe AllianceInfo -> KillboardStats -> [a]

combinedLookup :: ApiCombinator a -> [CharacterName] -> HttpClient (HttpClientResult [a])
combinedLookup combinator names =
    getCharacterID names >>=
      bindHttpClient handleNamesAndIds
  where
    handleNamesAndIds namesAndIds =
      fold <$> traverse handleId namesAndIds
    handleId (_, charId) = do
      info <- getCharacterInfo charId
      zkill <- getKillboardStats charId
      case (info, zkill) of
        (Left x, _)        -> return $ Left x
        (_, Left x)        -> return $ Left x
        (Right x, Right y) -> handleInfo charId x y
    handleInfo charId info zkill = do
      corp <- getCorporationInfo (ciCorporationId info)
      alliance <- getAllianceInfo (ciAllianceId info)
      return $ combinator <$> Right charId <*> Right info <*> corp <*> alliance <*> Right zkill
