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
  , dumpCaches
  , loadCaches
  , loadIOCaches
  )
where

import           Control.Concurrent.MSem     (MSem (..), new, signal, wait)
import           Control.Concurrent.MVar     (MVar (..), modifyMVar_, newMVar,
                                              readMVar)
import           Control.DeepSeq             (NFData, deepseq)
import           Control.Exception           (bracket_, catch, throw)
import           Control.Logging             (debug, timedDebug,
                                              withStdoutLogging)
import           Control.Monad.Free
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy        as L
import           Data.Either                 (partitionEithers)
import           Data.Foldable               (fold)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time.Clock             (DiffTime, UTCTime, diffUTCTime,
                                              getCurrentTime)
import           Data.Tuple                  (swap)
import           Eve.Api.Config
import           Eve.Api.Esi
import           Eve.Api.Http
import           Eve.Api.Types
import           Eve.Api.Xml
import           Eve.Api.Zkill
import           Formatting                  (int, sformat, stext, string, (%))
import           Network.HTTP.Client         (HttpException)
import           Network.HTTP.Client.CertMan (getURL)
import           System.IO.Unsafe            (unsafePerformIO)
import           Unsafe.Coerce               (unsafeCoerce)

-- | interprets the http client monad, performing real io
runHttpClientIO :: HttpClient a -> IO a
runHttpClientIO = iterM run where
  run :: HttpClientF (IO a) -> IO a
  run (HttpGet sem url f)           = getURLEither sem url >>= f
  run (GetCharacterIDChunk names f) = getCharIDChunkIO runHttpClientIO names >>= f
  run (DebugMessage msg f)          = debug msg >> f
  run (CachedID names f)            = cachedCharacterID names >>= f
  run (PutCachedID tuples f)        = putCachedCharacterID tuples >> f
  run (CachedCharacter k f)         = validCache characterInfoCache k >>= f
  run (PutCachedCharacter k v f)    = putCurrentCache characterInfoCache k v >> f
  run (CachedCorporation k f)       = validCache corporationInfoCache k >>= f
  run (PutCachedCorporation k v f)  = putCurrentCache corporationInfoCache k v >> f
  run (CachedAlliance k f)          = validCache allianceInfoCache k >>= f
  run (PutCachedAlliance k v f)     = putCurrentCache allianceInfoCache k v >> f
  run (CachedStats k f)             = validCache killboardStatCache k >>= f
  run (PutCachedStats k v f)        = putCurrentCache killboardStatCache k v >> f

guardIO :: MSem Int -> String -> IO a -> IO a
guardIO sem ctx =
  bracket_
    (wait sem >> debug (sformat ("acquired semaphore: " % string) ctx))
    (debug (sformat ("releasing semaphore: " % string) ctx) >> signal sem)

getCharIDChunkIO runner names = do
  debug (sformat ("looking up character ids: " % int % " names")
                      (length names))
  let url = characterIDUrl names
  result <- runner $ httpGet xmlApiSem url
  return $ case result of
    Left x   -> Left x
    Right bs -> parseXMLBody bs

getURLEither :: MSem Int -> String -> IO (HttpClientResult L.ByteString)
getURLEither sem url = guardIO sem url $ do
    bs <- getURL url
    return (Right bs)
  `catch`
    \ex -> (return . Left . HttpClientError) ex

-- | interprets the http client monad against a map of url and content
runTestIO :: M.Map String L.ByteString -> HttpClient a -> IO a
runTestIO ioCache = iterM run where
  run :: HttpClientF (IO a) -> IO a
  run (HttpGet sem url f)           = fakeURLEither ioCache sem url >>= f
  run (DebugMessage msg f)          = debug msg >> f
  run (CachedID names f)            = f ([], names)
  run (PutCachedID _ f)             = f
  run (CachedCharacter k f)         = f Nothing
  run (PutCachedCharacter k v f)    = f
  run (CachedCorporation k f)       = f Nothing
  run (PutCachedCorporation k v f)  = f
  run (CachedAlliance k f)          = f Nothing
  run (PutCachedAlliance k v f)     = f
  run (CachedStats k f)             = f Nothing
  run (PutCachedStats k v f)        = f
  run (GetCharacterIDChunk names f) = fakeCharIDChunkIO ioCache names >>= f

fakeURLEither :: M.Map String L.ByteString -> MSem Int -> String -> IO (HttpClientResult L.ByteString)
fakeURLEither ioCache sem url = guardIO sem url $
  return $ case M.lookup url ioCache of
    Nothing -> Left $ OtherError $ "not cached: " ++ url
    Just x  -> Right x

fakeCharIDChunkIO ioCache names = do
  results <- traverse fakeCharID names
  (return . fold) results
  where
    fakeCharID name =
      runTestIO ioCache (fmap (dec name) <$> httpGet xmlApiSem (characterIDUrl [name]))
    dec name bytes = [(name, (CharacterID . fromJust . decode) bytes)]

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

idByName :: MVar (M.Map CharacterName CharacterID)
{-# NOINLINE idByName #-}
idByName = unsafePerformIO $ do
  debug "initialized character id cache"
  newMVar M.empty

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

validCache mvar k = do
  m <- readMVar mvar
  now <- getCurrentTime
  return $ M.lookup k m >>= filterOutdated now

putCurrentCache mvar k v = do
  now <- getCurrentTime
  modifyMVar_ mvar $ return . M.insert k (Cached v now)
  return ()

cachedCharacterID names = do
  byName <- readMVar idByName
  return $ knownAndUnknown byName names

putCachedCharacterID tuples =
  modifyMVar_ idByName $ return . M.union (M.fromList tuples)

knownAndUnknown m ks =
  swap (partitionEithers (lookupEither m <$> ks))

lookupEither m k =
  case M.lookup k m of
    Nothing -> Left k
    Just a  -> Right (k, a)

cacheSeconds :: Double
cacheSeconds = 3*3600

filterOutdated now ca =
  if realToFrac delta < cacheSeconds
    then Just (cachedInfo ca)
    else Nothing
  where delta = diffUTCTime now (cachedTime ca)

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

dumpCache mvar fn = do
  m <- readMVar mvar
  L.writeFile fn $ encode m

loadCache mvar fn = do
  loaded  <- (fromJust . decode) <$> L.readFile fn
  modifyMVar_ mvar (return . M.union loaded)

-- | Loads the caches from the dump directory for runTestIO
loadIOCaches :: IO (M.Map String L.ByteString)
loadIOCaches = do
  ids <- (fromRight . eitherDecode) <$> L.readFile "dump/id_by_name.json" :: IO (M.Map CharacterName CharacterID)
  infos <- (fromRight . eitherDecode) <$> L.readFile "dump/characters.json" :: IO (M.Map CharacterID (Cached CharacterInfo))
  corps <- (fromRight . eitherDecode) <$> L.readFile "dump/corporations.json" :: IO (M.Map CorporationID (Cached CorporationInfo))
  alliances <- (fromRight . eitherDecode) <$> L.readFile "dump/alliances.json" :: IO (M.Map AllianceID (Cached AllianceInfo))
  killboard <- (fromRight . eitherDecode) <$> L.readFile "dump/killboards.json" :: IO (M.Map CharacterID (Cached Eve.Api.Zkill.KillboardStats))
  return $ Prelude.foldr M.union M.empty
    [ enc (\x -> characterIDUrl [x]) ids
    , enc charInfoUrl (cachedInfo <$> infos)
    , enc corpInfoUrl (cachedInfo <$> corps)
    , enc allianceInfoUrl (cachedInfo <$> alliances)
    , enc zkillStatUrl (cachedInfo <$> killboard)
    ]
  where
    enc f = M.map encode . M.mapKeys f

loadIOCache toUrl toBS fn = (M.map toBS . M.mapKeys toUrl . fromRight . eitherDecode) <$> L.readFile fn

fromRight x = case x of
  Left y  -> throw (userError ("fromRight: Left " ++ show y))
  Right y -> y
