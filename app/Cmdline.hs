{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Logging             (debug, withStdoutLogging)
import           Control.Monad               (mapM_)
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           Eve.Api.Config
import           Eve.Api.Http
import           Eve.Api.Types
import           Formatting                  (int, sformat, stext, (%))
import           Network.HTTP.Client.CertMan (setGlobalManagerFromPath)
import           Network.Socket              (withSocketsDo)
import           Prelude
import           System.Console.GetOpt
import           System.Environment          (getArgs)

data Flag = Test deriving (Show, Eq)

options :: [OptDescr Flag]
options = [ Option ['t'] ["test"] (NoArg Test) "use test runner (fixed set of test data)"]

parseOpts :: [String] -> IO ([Flag], [String])
parseOpts args =
  case getOpt Permute options args of
    (o, n, []) -> return (o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: hs-little-helper [OPTION...] files..."

main :: IO ()
main = withSocketsDo $ withStdoutLogging $ do
  setGlobalManagerFromPath certificateStore
  (_, args) <- getArgs >>= parseOpts
  let filenames = if null args then [ "short.x" ] else args
  mapM_ handleFile filenames
  return ()

handleFile :: FilePath -> IO ()
handleFile fn = do
  str <- TIO.readFile fn
  let names = characterName <$> T.lines str
  pilots <- combinedLookup names
  mapM_ handlePilot pilots

handlePilot :: PilotInfo -> IO ()
handlePilot pilot =
  debug $ sformat ("char:" % stext % " corp:" % stext % " alliance:" % stext % " kills:" % int)
                  (pilotName pilot)
                  (pilotCorporationName pilot)
                  (fromMaybe "" $ pilotAllianceName pilot)
                  (pilotRecentKills pilot)
