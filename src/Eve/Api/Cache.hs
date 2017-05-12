{-# LANGUAGE OverloadedStrings #-}

{- |
Module:      Eve.Api.Cache
Description: Cached Eve API
Copyright:   2017 Random J Farmer
License:     MIT
-}
module Eve.Api.Cache
  ( lookupCharacterIDs
  , lookupCharacterInfo
  , lookupCorporationInfo
  , lookupAllianceInfo
  , lookupKillboardStats
  , dumpCaches
  , loadCaches
  , CharacterInfo(..)
  , CorporationInfo(..)
  , AllianceInfo(..)
  , KillboardStats(..)
  , KillboardMonth(..)
  , ActivePvp(..)
  , ActivePvpShips(..)
  , ActivePvpSystems(..)
  , ActivePvpKills(..)
  , ZkillInfo(..)
  , Cached(..)
  ) where

import           Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_,
                                          newMVar, putMVar, readMVar, takeMVar)
import           Control.Logging         (debug, withStdoutLogging)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy    as L
import           Data.Either             (partitionEithers)
import qualified Data.Map.Strict         as M
import           Data.Maybe              (fromJust)
import           Data.Time.Clock         (DiffTime, UTCTime, diffUTCTime,
                                          getCurrentTime)
import           Data.Tuple              (swap)
import           Eve.Api.Esi             (AllianceInfo (..), CharacterInfo (..),
                                          CorporationInfo (..))
import qualified Eve.Api.Esi             as E
import           Eve.Api.Types
import qualified Eve.Api.Xml             as X
import           Eve.Api.Zkill           (ActivePvp (..), ActivePvpKills (..),
                                          ActivePvpShips (..),
                                          ActivePvpSystems (..),
                                          KillboardMonth (..),
                                          KillboardStats (..), ZkillInfo (..))
import qualified Eve.Api.Zkill           as Z
import           Formatting              (int, sformat, stext, (%))
import           GHC.Generics            (Generic)
import           System.IO.Unsafe        (unsafePerformIO)


knownAndUnknown:: Ord k => M.Map k a -> [k] -> ([(k,a)], [k])
knownAndUnknown m ks =
  swap (partitionEithers (lookupEither m <$> ks))

lookupEither :: Ord k => M.Map k a -> k -> Either k (k,a)
lookupEither m k =
  case M.lookup k m of
    Nothing -> Left k
    Just a  -> Right (k, a)

idByName :: MVar (M.Map CharacterName CharacterID)
{-# NOINLINE idByName #-}
idByName = unsafePerformIO $ do
  debug "initialized character name cache"
  newMVar M.empty

-- | Cached lookup of character ids
lookupCharacterIDs :: [CharacterName] -> IO [(CharacterName, CharacterID)]
lookupCharacterIDs names = do
  byName <- readMVar idByName
  let (known, unknown) = knownAndUnknown byName names
  debug $ sformat ("known/unknown: " % int % "/" % int) (length known) (length unknown)
  result <- X.lookupCharacterIDs unknown
  modifyMVar_ idByName (\byNameOld -> do
    let byNameNew = M.union byNameOld $ M.fromList result
    return byNameNew)
  return (known ++ result)


data Cached a =
  Cached
  { cachedInfo :: a
  , cachedTime :: UTCTime
  } deriving (Show)
instance ToJSON a => ToJSON (Cached a) where
  toJSON (Cached info time) = object ["cachedInfo" .= toJSON info, "cachedTime" .= toJSON time]
instance FromJSON a => FromJSON (Cached a) where
  parseJSON (Object v) = Cached <$>
                        v .: "cachedInfo" <*>
                        v .: "cachedTime"


cacheSeconds :: Double
cacheSeconds = 3*3600

ignoreAfter :: Double -> UTCTime -> (k, Cached a) -> Either k (k, Cached a)
ignoreAfter seconds now (charId, ca) =
    if realToFrac delta < seconds
      then Right (charId, ca)
      else Left charId
  where delta = diffUTCTime now (cachedTime ca)

type CacheMap k v = M.Map k (Cached v)
type CacheMVar k v = MVar (CacheMap k v)

characterInfoCache :: CacheMVar CharacterID CharacterInfo
{-# NOINLINE characterInfoCache #-}
characterInfoCache = unsafePerformIO $ do
  debug "characterInfoCache - initializing cache"
  newMVar M.empty

corporationInfoCache :: CacheMVar CorporationID CorporationInfo
{-# NOINLINE corporationInfoCache #-}
corporationInfoCache = unsafePerformIO $ do
  debug "corporationInfoCache - initializing cache"
  newMVar M.empty

allianceInfoCache :: CacheMVar AllianceID AllianceInfo
{-# NOINLINE allianceInfoCache #-}
allianceInfoCache = unsafePerformIO $ do
  debug "allianceInfoCache - initializing cache"
  newMVar M.empty

killboardStatCache :: CacheMVar CharacterID KillboardStats
{-# NOINLINE killboardStatCache #-}
killboardStatCache = unsafePerformIO $ do
  debug "killboardStatCache - initializing cache"
  newMVar M.empty


esiLookup :: Ord k => CacheMVar k v -> (k -> IO v) -> k -> IO v
esiLookup cache esiLookup k = do
  byId <- readMVar cache
  now <- getCurrentTime
  let cached = lookupEither byId k >>= ignoreAfter cacheSeconds now
  case cached of
    Left k -> do
      result <- esiLookup k
      modifyMVar_ cache (\byId -> do
        let byIdNew = M.insert k (Cached result now) byId
        return byIdNew)
      return result
    Right (k, Cached v _) -> return v

lookupCharacterInfo :: CharacterID -> IO CharacterInfo
lookupCharacterInfo = esiLookup characterInfoCache E.lookupCharacterInfo

lookupCorporationInfo :: CorporationID -> IO CorporationInfo
lookupCorporationInfo = esiLookup corporationInfoCache E.lookupCorporationInfo

lookupAllianceInfo :: AllianceID -> IO AllianceInfo
lookupAllianceInfo = esiLookup allianceInfoCache E.lookupAllianceInfo

lookupKillboardStats :: CharacterID -> IO KillboardStats
lookupKillboardStats = esiLookup killboardStatCache Z.lookupKillboardStats

dumpCaches :: IO ()
dumpCaches = do
  dumpCache idByName "dump/id_by_name.json"
  dumpCache characterInfoCache "dump/characters.json"
  dumpCache corporationInfoCache "dump/corporations.json"
  dumpCache allianceInfoCache "dump/alliances.json"
  dumpCache killboardStatCache "dump/killboards.json"

loadCaches :: IO ()
loadCaches = withStdoutLogging $ do
  loadCache idByName "dump/id_by_name.json"
  loadCache characterInfoCache "dump/characters.json"
  loadCache corporationInfoCache "dump/corporations.json"
  loadCache allianceInfoCache "dump/alliances.json"
  loadCache killboardStatCache "dump/killboards.json"

dumpCache mvar fn = do
  m <- readMVar mvar
  L.writeFile fn $ encode m

loadCache mvar fn = do
  loaded  <- (fromJust . decode) <$> L.readFile fn
  modifyMVar_ mvar (return . M.union loaded)
