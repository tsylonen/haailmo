{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Handlers (static, formHandler, showState) where

import Data.Monoid (mconcat)

import Model
import Happstack.Server
import Data.Text.Lazy (Text)
import Data.Acid            ( AcidState )
import Data.Acid.Advanced   ( query', update' )
import Control.Monad (forM_)


import Text.Blaze.Html5 (Html, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


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
  method [POST]
  rsvps <- mapM getRsvpByPostfix ["1", "2", "3", "4", "5", "6"]
  other <- lookText "muuta"
  let rsvps' = filter (\r -> getName r /= "") rsvps
      rsvpgroup = RsvpGroup rsvps' other
  update' acid (AddRsvpGroup rsvpgroup)
  seeOther ("kiitos" :: String) (toResponse())

showState :: AcidState AppState -> ServerPart Response
showState acid = do
  groups <- query' acid PeekState
  ok $ toResponse $ stateView groups
  
stateView :: [RsvpGroup] -> Html
stateView groups =
  H.docTypeHtml $
  H.ul $
  mconcat $ map (H.li . renderRsvpGroup) groups
    

    

renderRsvpGroup :: RsvpGroup -> Html
renderRsvpGroup RsvpGroup{..} = do
  H.ul $
      mconcat [H.li $ renderRsvp r | r <- getRsvps]
  H.p $ toHtml getOther
  
renderRsvp :: Rsvp -> Html
renderRsvp Rsvp{..} = do
  toHtml getName
  toHtml (", " :: String)
  toHtml getComing
  toHtml (", " :: String)
  toHtml getDiet

  
  
