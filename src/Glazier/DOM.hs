-- | NB. Glazier.DOM.Event.* are not exported due to duplicate record fields
-- It is up to the user to import the Event modules as required.
module Glazier.DOM
    ( module Glazier.DOM.EventTarget
    , module Glazier.DOM.Event
    ) where

import Glazier.DOM.Event
import Glazier.DOM.EventTarget

import Data.Coerce
import Glazier.DOM.EventTarget.Node

wack :: Node -> EventTarget
wack = fromNode





