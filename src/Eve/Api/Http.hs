{- |
Module:      Eve.Api.Http
Description: Eve Api HTTP IO
Copyright:   2017 Random J Farmer
License:     MIT
-}

module Eve.Api.Http
  ( combinedLookup
  , httpGet
  , getCharacterID
  , getCharacterInfo
  , getAllianceInfo
  , getCorporationInfo
  , getKillboardStats
  , dumpCaches
  , loadCaches
  )
where

import           Control.Applicative         (empty)
import           Control.Concurrent.Async    (concurrently, mapConcurrently)
import           Control.Concurrent.MSem     (MSem, new, signal, wait)
import           Control.Concurrent.MVar     (MVar, modifyMVar_, newMVar,
                                              readMVar)
import           Control.Exception           (bracket_, throw)
import           Control.Logging             (debug, timedDebug,
                                              withStdoutLogging)
import           Data.Aeson
import qualified Data.ByteString.Lazy        as L
import           Data.Either                 (partitionEithers)
import           Data.Foldable               (fold)
import           Data.List.Split             (chunksOf)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.Text                   (Text)
import           Data.Time.Clock             (UTCTime, diffUTCTime,
                                              getCurrentTime)
import           Data.Tuple                  (swap)
import           Eve.Api.Config
import           Eve.Api.Esi
import           Eve.Api.Types
import           Eve.Api.Xml
import           Eve.Api.Zkill
import           Formatting                  (int, sformat, (%))
import           Network.HTTP.Client.CertMan (getURL)
import           Prelude
import           System.IO.Unsafe            (unsafePerformIO)

guardIO :: MSem Int -> IO a -> IO a
guardIO sem =
  bracket_
    (wait sem)
    (signal sem)

httpGet :: MSem Int -> Text -> String -> IO L.ByteString
httpGet sem msg url = guardIO sem $ timedDebug msg $ getURL url

cachedGet :: MSem Int -> Text -> IO (Maybe a) -> (a -> IO ()) -> (L.ByteString -> a) -> String -> IO a
cachedGet sem msg cacheLookup cacheEnter decodeFn url = guardIO sem $ do
    cached <- cacheLookup
    case cached of
      Just x -> return x
      Nothing -> do
        result <- timedDebug msg (decodeFn <$> getURL url)
        cacheEnter result
        return result


getClientResult :: Ord k => MVar (M.Map k (Cached a)) -> MSem Int -> Text -> (k -> String) -> (L.ByteString -> a) -> k -> IO a
getClientResult cache sem msg toUrl decodeResult k = do
  let cacheLookup = validCache cache k
  let cacheEnter = putCurrentCache cache k
  cached <- cacheLookup
  case cached of
    Just x -> return x
    Nothing -> cachedGet sem msg cacheLookup cacheEnter decodeResult (toUrl k)

validCache :: Ord k => MVar (M.Map k (Cached b)) -> k -> IO (Maybe b)
validCache mvar k = do
  m <- readMVar mvar
  now <- getCurrentTime
  return $ M.lookup k m >>= filterOutdated now

putCurrentCache :: Ord k => MVar (M.Map k (Cached a)) -> k -> a -> IO ()
putCurrentCache mvar k v = do
  now <- getCurrentTime
  modifyMVar_ mvar $ return . M.insert k (Cached v now)
  return ()

cacheSeconds :: Double
cacheSeconds = 3*3600

filterOutdated :: UTCTime -> Cached a -> Maybe a
filterOutdated now ca =
  if realToFrac delta < cacheSeconds
    then Just (cachedInfo ca)
    else Nothing
  where delta = diffUTCTime now (cachedTime ca)

-- | Helper to decode a client result
decodeClientResult :: FromJSON a => String -> L.ByteString -> a
decodeClientResult errCtx bs =
  case eitherDecode bs of
      Left str -> throw $ DecodeError (errCtx ++ ": " ++ str)
      Right a  -> a

getCharacterInfo :: CharacterID -> IO CharacterInfo
getCharacterInfo k =
  getClientResult characterInfoCache esiApiSem
                  (sformat ("looking up character " % int) (_characterID k))
                  charInfoUrl
                  (decodeClientResult (show k))
                  k

getCorporationInfo :: CorporationID -> IO CorporationInfo
getCorporationInfo k =
  getClientResult corporationInfoCache esiApiSem
                (sformat ("looking up corporation " % int) (_corporationID k))
                corpInfoUrl
                (decodeClientResult (show k))
                k

getAllianceInfo :: Maybe AllianceID -> IO (Maybe AllianceInfo)
getAllianceInfo Nothing = return Nothing
getAllianceInfo (Just k) = do
  result <- getClientResult allianceInfoCache esiApiSem
                (sformat ("looking up alliance " % int) (_allianceID k))
                allianceInfoUrl
                (decodeClientResult (show k))
                k
  return $ Just result

getKillboardStats :: CharacterID -> IO KillboardStats
getKillboardStats k =
  getClientResult killboardStatCache zkillApiSem
                (sformat ("looking up killboard " % int) (_characterID k))
                zkillStatUrl
                (decodeClientResult (show k))
                k


-- | get the character ids for the named characters
getCharacterID :: [CharacterName] -> IO [(CharacterName, CharacterID)]
getCharacterID names = do
  (known, unknown) <- cachedCharacterID names
  debug $ sformat ("known/unknown: " % int % "/" % int) (length known) (length unknown)
  let chunks = chunksOf xmlApiChunkSize unknown
  result <- fold <$> mapConcurrently getCharacterIDChunk chunks
  putCachedCharacterID result
  return $ known ++ result

cachedCharacterID :: [CharacterName] -> IO ([(CharacterName, CharacterID)], [CharacterName])
cachedCharacterID names = do
  byName <- readMVar idByName
  return $ knownAndUnknown byName names

putCachedCharacterID :: [(CharacterName, CharacterID)] -> IO ()
putCachedCharacterID tuples =
  modifyMVar_ idByName $ return . M.union (M.fromList tuples)

knownAndUnknown :: Ord a => M.Map a t -> [a] -> ([(a, t)], [a])
knownAndUnknown m ks =
  swap (partitionEithers (lookupEither m <$> ks))

lookupEither :: Ord t1 => M.Map t1 t -> t1 -> Either t1 (t1, t)
lookupEither m k =
  case M.lookup k m of
    Nothing -> Left k
    Just a  -> Right (k, a)

getCharacterIDChunk :: [CharacterName] -> IO [(CharacterName, CharacterID)]
getCharacterIDChunk names = do
  let msg = sformat ("looking up character ids: " % int % " names")
                    (length names)
  let url = characterIDUrl names
  parseXMLBody <$> httpGet xmlApiSem msg url

combinedLookup :: [CharacterName] -> IO [PilotInfo]
combinedLookup names = do
    tuples <- getCharacterID names
    mapConcurrently handleId tuples
  where
    handleId (_, charId) = do
      (info, zkill) <- concurrently (getCharacterInfo charId)
                                    (getKillboardStats charId)
      -- debug $ sformat ("killboard: " % shown) (ksactivepvp zkill)
      handleInfo charId info zkill
    handleInfo charId info zkill = do
      (corp, alliance) <- concurrently (getCorporationInfo (ciCorporationId info))
                                       (getAllianceInfo (ciAllianceId info))
      return $ pilotInfo charId info corp alliance zkill


pilotInfo :: CharacterID -> CharacterInfo -> CorporationInfo -> Maybe AllianceInfo -> KillboardStats -> PilotInfo
pilotInfo charId info corp alliance killboard =
  PilotInfo { pilotName = ciName info
            , pilotID = charId
            , pilotCorporationName = coCorporationName corp
            , pilotCorporationID = ciCorporationId info
            , pilotAllianceName = aiAllianceName <$> alliance
            , pilotAllianceID = ciAllianceId info
            , pilotFactionName = coFaction corp
            , pilotRecentKills = fromMaybe 0 $ apkillcount <$> (ksactivepvp killboard >>= apkills)
            }

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

type CacheMap k v = M.Map k (Cached v)
type CacheMVar k v = MVar (CacheMap k v)

-- | Cached information with a cache lifetime
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
  parseJSON _ = empty

idByName :: MVar (M.Map CharacterName CharacterID)
{-# NOINLINE idByName #-}
idByName = unsafePerformIO $ do
  debug "initialized character id cache"
  newMVar M.empty

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

-- | dump the caches to the dump directory
dumpCaches :: IO ()
dumpCaches = do
  dumpCache idByName "dump/id_by_name.json"
  dumpCache characterInfoCache "dump/characters.json"
  dumpCache corporationInfoCache "dump/corporations.json"
  dumpCache allianceInfoCache "dump/alliances.json"
  dumpCache killboardStatCache "dump/killboards.json"

-- | Load the caches from the dump directory
loadCaches :: IO ()
loadCaches = withStdoutLogging $ do
  loadCache idByName "dump/id_by_name.json"
  loadCache characterInfoCache "dump/characters.json"
  loadCache corporationInfoCache "dump/corporations.json"
  loadCache allianceInfoCache "dump/alliances.json"
  loadCache killboardStatCache "dump/killboards.json"

dumpCache :: ToJSON a => MVar a -> FilePath -> IO ()
dumpCache mvar fn = do
  m <- readMVar mvar
  L.writeFile fn $ encode m

loadCache :: (FromJSON a, Ord k, FromJSONKey k) => MVar (M.Map k a) -> FilePath -> IO ()
loadCache mvar fn = do
  loaded  <- (fromJust . decode) <$> L.readFile fn
  modifyMVar_ mvar (return . M.union loaded)
