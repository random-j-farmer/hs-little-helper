{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
Module:      Eve.Api.FreeHttp
Description: Free HTTP Client
Copyright:   2017 Random J Farmer
License:     MIT
-}

module Eve.Api.FreeHttp where

import           Control.Exception           (catch, throw)
import           Control.Monad.Free
import           Control.Monad.Free.TH       (makeFree)
import           Data.Aeson                  (FromJSON (..), eitherDecode,
                                              encode)
import qualified Data.ByteString.Lazy        as L
import           Data.Foldable               (fold)
import           Data.List.Split             (chunksOf)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (fromJust)
import           Data.Typeable               (Typeable)
import           Eve.Api.Cache               (Cached (..))
import           Eve.Api.Config
import           Eve.Api.Esi
import           Eve.Api.Types
import           Eve.Api.Xml
import           Eve.Api.Zkill
import           Network.HTTP.Client         (HttpException)
import           Network.HTTP.Client.CertMan (getURL)
import           Text.Show.Functions


-- | DSL primitives
data HttpClientF next
  = HttpGet String (HttpClientResult L.ByteString -> next)
  -- test version must use different implementation
  | GetCharacterIDChunk [CharacterName] (HttpClientResult [(CharacterName, CharacterID)] -> next)
  deriving (Show, Functor)

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
getCharacterInfo charId = do
  result <- httpGet (charInfoUrl charId)
  return $ decodeClientResult result

getCorporationInfo :: CorporationID -> HttpClient (HttpClientResult CorporationInfo)
getCorporationInfo corpId = do
  result <- httpGet (corpInfoUrl corpId)
  return $ decodeClientResult result

decodeClientResult :: FromJSON a => HttpClientResult L.ByteString -> HttpClientResult a
decodeClientResult result =
  case result of
    Left x -> Left x
    Right bs -> case eitherDecode bs of
      Left str -> Left $ DecodeError str
      Right a  -> Right a

runHttpClientIO :: HttpClient a -> IO a
runHttpClientIO = iterM run where
  run :: HttpClientF (IO a) -> IO a
  run (HttpGet url f)               = getURLEither url >>= f
  run (GetCharacterIDChunk names f) = getCharIDChunk names >>= f

getCharIDChunk names = do
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

runTestIO :: M.Map String L.ByteString -> HttpClient a -> IO a
runTestIO ioCache = iterM run where
  run :: HttpClientF (IO a) -> IO a
  run (HttpGet url f) = fakeURLEither ioCache url >>= f
  run (GetCharacterIDChunk names f) = do
    results <- traverse (\x -> getCharIDChunk [x]) names
    -- sequence ((\x -> getCharIDChunk [x]) <$> names)
    f $ fold results

fakeURLEither :: M.Map String L.ByteString -> String -> IO (HttpClientResult L.ByteString)
fakeURLEither ioCache url =
    return $ case M.lookup url ioCache of
      Nothing -> Left $ OtherError $ "not cached: " ++ url
      Just x  -> Right x

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
