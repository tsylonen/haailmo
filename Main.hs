module Main where

import Model
import Handlers

import Happstack.Server

import Control.Monad (msum)
import System.Environment(getArgs)

import Control.Exception    ( bracket )
import Data.Acid            ( AcidState, openLocalState )
import Data.Acid.Advanced   ( query')
import Data.Acid.Local      ( createCheckpointAndClose )

main :: IO ()
main = do
  args <- getArgs
  let p = read . head $ args
      conf = Conf p Nothing Nothing 1000 Nothing 
  runAcid conf

acidApp :: AcidState AppState -> ServerPart Response
acidApp acid = do
  decodeBody $ defaultBodyPolicy "/tmp/" 0 1000 1000
  state <- query' acid PeekState  
  msum [
    dir "form" $ formHandler acid
    ,dir "state" $ ok $ toResponse $ show state
    , dir "kiitos" $ serveFile (asContentType "text/html") "static/kiitos.html"
    ,static
    ]

runAcid :: Conf -> IO ()
runAcid c =
  bracket (openLocalState initialAppState)
  createCheckpointAndClose
  (simpleHTTP c . acidApp)
