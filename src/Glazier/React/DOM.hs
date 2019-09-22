-- | NB. Glazier.React.DOM.Event.* are not exported due to duplicate record fields
-- It is up to the user to import the Event modules as required.
module Glazier.React.DOM
    ( module Glazier.React.DOM.EventTarget
    , module Glazier.React.DOM.Event
    ) where

import Glazier.React.DOM.Event
import Glazier.React.DOM.EventTarget

import Data.Coerce
import Glazier.React.DOM.EventTarget.Node

wack :: Node -> EventTarget
wack = fromNode





