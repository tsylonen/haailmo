{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards
  ,ScopedTypeVariables, OverloadedStrings#-}

module Main where

import Happstack.Lite
import Data.Text.Lazy (Text)

import Control.Applicative  ( (<$>) )
import Control.Exception    ( bracket )
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Data            ( Data, Typeable )
import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy )

data Rsvp = Rsvp {getName :: Text, getComing :: Bool, getDiet :: Text}
            deriving (Read, Show, Data, Typeable)

data RsvpGroup = RsvpGroup {getRsvps :: [Rsvp], getOther :: Text}
                 deriving (Read, Show, Data, Typeable)

data AppState = AppState {getRsvpGroups :: [RsvpGroup]}
                deriving (Read, Show, Data, Typeable)

initialAppState :: AppState
initialAppState = AppState []

$(deriveSafeCopy 0 'base ''Rsvp)
$(deriveSafeCopy 0 'base ''RsvpGroup)
$(deriveSafeCopy 0 'base ''AppState)

addRsvpGroup :: RsvpGroup -> Update AppState [RsvpGroup]
addRsvpGroup g =
  do state@AppState{..} <- get
     let newgroups = g:getRsvpGroups
     put $ state {getRsvpGroups = newgroups}
     return newgroups

peekState :: Query AppState [RsvpGroup]
peekState = getRsvpGroups <$> ask

$(makeAcidic ''AppState ['addRsvpGroup, 'peekState])

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
--  return $ toResponse (show rsvpgroup)
  seeOther ("kiitos" :: String) (toResponse())
