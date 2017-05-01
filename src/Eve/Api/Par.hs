{-# LANGUAGE OverloadedStrings #-}
{- |
Module:      Eve.Api.Par
Description: Parallel mapping using async and a guarding semaphore
Copyright:   (c) 2017 Random J. Farmer
LICENSE:     MIT

async's mapConcurrently does all the asyncs at the same time.
We want to limit the numer of parallel connections to the same host,
so the resource is guarded by a semaphore.

-}
module Eve.Api.Par (parmap)
where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Concurrent.MSem  (MSem, signal, wait)
import           Control.Exception.Base   (bracket)
import           Control.Logging          (debug)

parmap :: Traversable t => MSem Int -> (a -> IO b) -> t a -> IO (t b)
parmap sem = mapConcurrently . guarded sem

guarded :: MSem Int -> (a -> IO b) -> (a -> IO b)
guarded sem fun a =
  bracket
    (waitAndReturn sem)
    signalAndLog
    (\_ -> fun a)

waitAndReturn :: MSem Int -> IO (MSem Int)
waitAndReturn sem = do
  wait sem
  debug "acquired semaphore"
  return sem

signalAndLog :: MSem Int -> IO ()
signalAndLog sem = do
  signal sem
  debug "released semaphore"
