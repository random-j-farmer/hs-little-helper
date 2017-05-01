{-# LANGUAGE OverloadedStrings #-}

{- |
Module:      Eve.Api.Config
Description: Configuration for Eve.Api packages
Copyright:   (c) 2017 Random J Farmer
License:     MIT

The configurion is done via Environment variables.
-}

module Eve.Api.Config ( certificateStore
                      , xmlApiConnections
                      , xmlApiChunkSize
                      , esiConnections
                      , esiDatasource
                      )
where

import           Data.Maybe         (fromMaybe)
import           System.Environment (lookupEnv)
import           System.IO.Unsafe   (unsafePerformIO)

{-# NOINLINE maybeEnv #-}
maybeEnv varname = unsafePerformIO $ lookupEnv varname
env varname def = fromMaybe def $ maybeEnv varname

-- | Path for certificate store - CERT_STORE
--
-- The certificate store is a directory with PEM public key
-- root certificates.  If given, all https connections will
-- only be allowed for servers whose certificate is signed
-- by one of the certificates in the store.
--
-- If the environment variable CERT_STORE is not given,
-- the hosts default certificate database will be used.

certificateStore :: Maybe FilePath
certificateStore = maybeEnv "CERT_STORE"

-- | Number of parallel XML Api connections - XML_API_CONNECTIONS
--
-- If no limit is given, 4 parallel connections will be used.
--
-- XML Api is only used for bulk character id lookups.
--
xmlApiConnections :: Int
xmlApiConnections = read $ env "XML_API_CONNECTIONS" "4"

-- | Chunk size for XML Api Character ID Lookups - XML_API_CHUNK_SIZE
--
-- Number of character ids that are resolved in one xml api call
-- (== HTTP GET).  Default 50
xmlApiChunkSize :: Int
xmlApiChunkSize = read $ env "XML_API_CHUNK_SIZE" "50"

-- | Number of parallel ESI connections - ESI_CONNECTIONS
--
-- If no limit is given, 4 parallel connections will be used.
--
-- This will only be used for esiParMap (all other calls
-- are stricly 1 request, 1 response), but esiParMap can
-- be used to parallelize most calls.
--
esiConnections :: Int
esiConnections = read $ env "ESI_CONNECTIONS" "4"

-- | Ese Datasource - ESI_DATASOURCE
--
-- Defaults to "tranquility"
esiDatasource :: String
esiDatasource = env "ESI_DATASOURCE" "tranquility"
