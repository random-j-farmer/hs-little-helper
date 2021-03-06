module Handler.Pilots where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Eve.Api.Http
import Eve.Api.Types
import Data.Text (strip)

-- Define our data that will be used for creating the form.
newtype PilotsForm = PilotsForm
    { pilotsArea :: Textarea
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getPilotsR :: Handler Html
getPilotsR = do
    (formWidget, formEnctype) <- generateFormPost pilotsForm
    let infos = [] :: [PilotInfo]
    defaultLayout $ do
        setTitle "Random's Little Helper"
        $(widgetFile "pilots")

postPilotsR :: Handler TypedContent
postPilotsR = do
  ((result, formWidget), formEnctype) <- runFormPostNoToken pilotsForm
  infos <- case result of
          FormSuccess res ->
            liftIO (sortOn (negate . pilotRecentKills) <$> pilotInfos res)
          _ -> return []
  selectRep $ do
    provideRep $ do
      addHeader "Access-Control-Allow-Origin" "*"
      addHeader "Access-Control-Allow-Methods" "POST"
      returnJson infos
    provideRep $
      defaultLayout $ do
        setTitle "Random's Little Helper"
        $(widgetFile "pilots")

pilotInfos :: PilotsForm -> IO [PilotInfo]
pilotInfos form = do
  let names = (lines . unTextarea . pilotsArea) form
  combinedLookup $ characterName . strip <$> names

pilotsForm :: Form PilotsForm
pilotsForm = renderBootstrap3 BootstrapBasicForm $ PilotsForm
    <$> areq textareaField pilotSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where
      pilotSettings = FieldSettings
            { fsLabel = "Pilot Names"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "names"
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "...")
                ]
            }
