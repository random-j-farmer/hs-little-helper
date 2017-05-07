{-# LANGUAGE OverloadedStrings #-}

{- |
Module:      Eve.Api.Cache
Description: Cached Eve API
Copyright:   2017 Random J Farmer
License:     MIT
-}
module Eve.Api.Cache where

import           Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_,
                                          newMVar, putMVar, readMVar, takeMVar)
import           Control.Logging         (debug)
import           Data.Either             (partitionEithers)
import qualified Data.Map.Strict         as M
import           Data.Time.Clock         (DiffTime, UTCTime, diffUTCTime,
                                          getCurrentTime)
import           Data.Tuple              (swap)
import qualified Eve.Api.Esi             as E
import           Eve.Api.Types
import qualified Eve.Api.Xml             as X
import           Formatting              (int, sformat, stext, (%))
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
  }

cacheSeconds :: Double
cacheSeconds = 3*3600

ignoreAfter :: Double -> UTCTime -> (CharacterID, Cached a) -> Either CharacterID (CharacterID, Cached a)
ignoreAfter seconds now (charId, ca) =
    if realToFrac delta < seconds
      then Right (charId, ca)
      else Left charId
  where delta = diffUTCTime now (cachedTime ca)

characterInfoCache :: MVar (M.Map CharacterID (Cached CharacterInfo))
{-# NOINLINE characterInfoCache #-}
characterInfoCache = unsafePerformIO $ do
  debug "characterInfoCache - initializing cache"
  newMVar M.empty

lookupCharacterInfo :: CharacterID -> IO CharacterInfo
lookupCharacterInfo charId = do
  byId <- readMVar characterInfoCache
  now <- getCurrentTime
  let cached = lookupEither byId charId >>= ignoreAfter cacheSeconds now
  case cached of
    Left k -> do
      result <- E.lookupCharacterInfo k
      modifyMVar_ characterInfoCache (\byId -> do
        let byIdNew = M.insert k (Cached result now) byId
        return byIdNew)
      return result
    Right (k, Cached ci _) -> return ci
