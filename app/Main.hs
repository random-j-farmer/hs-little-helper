{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception           (throw)
import           Control.Logging             (debug, log, withStdoutLogging, errorL)
import           Control.Monad               (mapM_)
import           Data.Aeson                  (encode)
import qualified Data.ByteString.Lazy        as LB
import qualified Data.ByteString.Lazy.UTF8   as U
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (fromJust, fromMaybe)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import qualified Data.Text.Lazy              as LT
import           Eve.Api.Config
import           Eve.Api.Esi
import           Eve.Api.Http
import           Eve.Api.Interp
import           Eve.Api.Types
import           Eve.Api.Xml
import           Eve.Api.Zkill
import           Formatting                  (int, sformat, stext, (%), shown)
import           Network.HTTP.Client.CertMan (setGlobalManagerFromPath)
import           Network.Socket              (withSocketsDo)
import           System.Environment          (getArgs, lookupEnv)
import System.Console.GetOpt

data Flag = Test deriving (Show, Eq)

options :: [OptDescr Flag]
options = [ Option ['t'] ["test"] (NoArg Test) "use test runner (fixed set of test data)"]

parseOpts :: [String] -> IO ([Flag], [String])
parseOpts args =
  case getOpt Permute options args of
    (o, n, []) -> return (o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: hs-little-helper [OPTION...] files..."

testRunner :: HttpClient a -> IO a
testRunner client = do
  cache <- loadIOCaches
  runTestIO cache client

main :: IO ()
main = withSocketsDo $ withStdoutLogging $ do
  setGlobalManagerFromPath certificateStore
  (opts, args) <- getArgs >>= parseOpts
  let runner = if Test `elem` opts
                  then testRunner
                  else runHttpClientIO
  let filenames = if null args then [ "short.x" ] else args
  mapM_ (handleFile runner) filenames
  return ()

tupleCombinator :: ApiCombinator (CharacterID, CharacterInfo, CorporationInfo, Maybe AllianceInfo, KillboardStats)
tupleCombinator charId info corp alliance killboard =
  let tup = (charId, info, corp, alliance, killboard)
  in [tup]

handleFile runner fn = do
  str <- TIO.readFile fn
  let names = characterName <$> T.lines str
  tuples <- runner (combinedLookup tupleCombinator names)
  case tuples of
    Left x ->
      errorL $ sformat ("combined result: " % shown) tuples
    Right _ ->
      mapM_ (mapM_ handleTuple) tuples

handleTuple :: (CharacterID, CharacterInfo, CorporationInfo, Maybe AllianceInfo, KillboardStats) -> IO ()
handleTuple (charId, info, corp, alliance, stats) =
  debug $ sformat ("char:" % stext % " corp:" % stext % " alliance:" % stext % " kills:" % int % " losses:" % int)
                  (ciName info)
                  (coCorporationName corp)
                  (fromMaybe "" $ aiAllianceName <$> alliance)
                  (fromMaybe 0 $ ksshipsDestroyed stats)
                  (fromMaybe 0 $ ksshipsLost stats)
