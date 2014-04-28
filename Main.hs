{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite

main :: IO ()
main = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum [
  dir "form" formHandler
  , static
  ]
static :: ServerPart Response
static =
  serveDirectory EnableBrowsing ["sivu.html"] "static"

formHandler :: ServerPart Response
formHandler = undefined
