{-# LANGUAGE OverloadedStrings #-}

module Main where

import Model

import Happstack.Lite
import Data.Text.Lazy (Text)

import Control.Exception    ( bracket )
import Data.Acid            ( AcidState, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )

main :: IO ()
main = runAcid

acidApp :: AcidState AppState -> ServerPart Response
acidApp acid = do
  state <- query' acid PeekState  
  msum [
    dir "form" $ formHandler acid
    ,dir "state" $ ok $ toResponse $ show state
    , dir "kiitos" $ serveFile (asContentType "text/html") "static/kiitos.html"
    ,static
    ]

runAcid :: IO ()
runAcid =
  bracket (openLocalState initialAppState)
  createCheckpointAndClose
  (serve Nothing . acidApp)

static :: ServerPart Response
static =
  serveDirectory EnableBrowsing ["sivu.html"] "static"

textToBool :: Text -> Bool
textToBool "1" = True
textToBool _   = False

-- get rsvp from form
getRsvp :: String -> String -> String -> ServerPart Rsvp
getRsvp namefield rsvpfield dietfield = do
  name <- lookText namefield
  rsvpstr <- lookText rsvpfield
  diet <- lookText dietfield
  return $ Rsvp name (textToBool rsvpstr) diet

getRsvpByPostfix :: String -> ServerPart Rsvp
getRsvpByPostfix pfix = getRsvp n r d
  where n = "nimi" ++ pfix
        r = "rsvp" ++ pfix
        d = "valio" ++ pfix

formHandler :: AcidState AppState -> ServerPart Response
formHandler acid = do
  method [POST, GET]
  rsvps <- mapM getRsvpByPostfix ["1", "2", "3", "4", "5", "6"]
  other <- lookText "muuta"
  let rsvps' = filter (\r -> getName r /= "") rsvps
      rsvpgroup = RsvpGroup rsvps' other
  _ <- update' acid (AddRsvpGroup rsvpgroup)
  seeOther ("kiitos" :: String) (toResponse())
