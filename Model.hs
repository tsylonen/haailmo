{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards
  ,ScopedTypeVariables, OverloadedStrings#-}

module Model where

import Data.Text.Lazy (Text)

import Control.Applicative  ( (<$>) )
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Data            ( Data, Typeable )
import Data.Acid            ( Query, Update
                            , makeAcidic )
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
