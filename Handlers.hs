{-# LANGUAGE OverloadedStrings #-}

module Handlers (static, formHandler) where

import Model
import Happstack.Server
import Data.Text.Lazy (Text)
import Data.Acid            ( AcidState )
import Data.Acid.Advanced   ( query', update' )


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
