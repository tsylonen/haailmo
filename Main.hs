{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Happstack.Lite
import Data.Text.Lazy (pack, Text)

data Rsvp = Rsvp {getName :: Text, getComing :: Bool, getDiet :: Text}
            deriving (Show)

data RsvpGroup = RsvpGroup {getRsvps :: [Rsvp], getOther :: Text}
                 deriving (Show)

main :: IO ()
main = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum [
  dir "form" formHandler
  , dir "kiitos" $ serveFile (asContentType "text/html") "static/kiitos.html"
  , static
  ]
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

formHandler :: ServerPart Response
formHandler = do
  method [POST, GET]
  rsvps <- mapM getRsvpByPostfix ["1", "2", "3", "4", "5", "6"]
  other <- lookText "muuta"
  let rsvps' = filter (\r -> getName r /= "") rsvps
      rsvpgroup = RsvpGroup rsvps' other
  return $ toResponse (show rsvpgroup)

