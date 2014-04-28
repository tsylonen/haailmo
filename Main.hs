{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Happstack.Lite
import Data.Text.Lazy (pack, Text)

data Rsvp = Rsvp {name :: Text, coming :: Bool, diet :: Text}
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

formHandler :: ServerPart Response
formHandler = do
  method [POST, GET]
  name1 <- lookText "nimi1"
  rsvp1 <- lookText "rsvp1"
  diet1 <- lookText "valio1"
  let t = Rsvp name1 (textToBool rsvp1) diet1
  other <- lookText "muuta"
  return $ toResponse (show t)


  

