{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Control.Applicative ((<$>), (<*>))
import           Control.Logging     (withStdoutLogging)
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Eve.Api.Http
import           Eve.Api.Types
import           Yesod

data LittleHelper = LittleHelper

mkYesod "LittleHelper" [parseRoutes|
/ HomeR GET
/pilots PilotsR GET POST
|]

instance Yesod LittleHelper

-- Tells our application to use the standard English messages.
-- If you want i18n, then you can supply a translating function instead.
instance RenderMessage LittleHelper FormMessage where
  renderMessage _ _ = defaultFormMessage

data Pilots = Pilots
  { pilotsLocation :: Text
  , pilotsNames    :: Textarea }
  deriving Show

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Random's Little Helper"
  [whamlet|
<h1>Random's Little Helper
<ul>
  <li><a href=@{PilotsR}>Pilots</a>
|]

pilotsForm :: Html -> MForm Handler (FormResult Pilots, Widget)
pilotsForm = renderDivs $ Pilots
  <$> areq textField "Location" Nothing
  <*> areq textareaField "Names" Nothing

getPilotsR :: Handler Html
getPilotsR = do
  (widget, enctype) <- generateFormPost pilotsForm
  defaultLayout
    [whamlet|
      <form method=post action=@{PilotsR} enctype=#{enctype}>
        ^{widget}
        <br>
        <button>Submit
    |]

postPilotsR :: Handler Html
postPilotsR = do
  ((result, widget), enctype) <- runFormPost pilotsForm
  case result of
    FormSuccess pilots -> do
      let names = characterName . T.strip <$> T.lines (unTextarea $ pilotsNames pilots)
      pilotInfos <- liftIO $ combinedLookup names
      defaultLayout [whamlet|
                      <ul>
                        $forall pilot <- pilotInfos
                          <li>#{pilotName pilot} - #{pilotCorporationName pilot} - #{fromMaybe "" $ pilotAllianceName pilot}
                    |]
    _ -> defaultLayout
            [whamlet|
              <p>Oopsie!
              <form method=post action=@{PilotsR} enctype=#{enctype}>
                ^{widget}
                <button>Submit
            |]


main :: IO ()
main = withStdoutLogging $ warp 3000 LittleHelper
