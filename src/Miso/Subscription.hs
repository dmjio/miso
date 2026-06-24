-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Subscription" is the re-export hub for all of miso's built-in
-- 'Miso.Effect.Sub' subscriptions. A subscription is a long-running 'IO'
-- action of type @'Miso.Effect.Sink' action -> IO ()@ that delivers
-- external events — mouse moves, key presses, URL changes, animation
-- frames — into the update loop by calling its 'Miso.Effect.Sink'.
--
-- Register subscriptions in the 'Miso.Types.subs' field of a
-- 'Miso.Types.Component':
--
-- @
-- import "Miso"
-- import "Miso.Subscription"
--
-- myComponent = ('Miso.component' model update view)
--   { 'Miso.Types.subs' =
--       [ 'mouseSub'    MouseMoved
--       , 'keyboardSub' KeyPressed
--       , 'uriSub'      UrlChanged
--       , 'rAFSub'      Tick
--       ]
--   }
-- @
--
-- = Subscription catalogue
--
-- ['mouseSub'] global @pointermove@ — "Miso.Subscription.Mouse"
-- ['keyboardSub'] global @keydown@ \/ @keyup@ — "Miso.Subscription.Keyboard"
-- ['arrowsSub', 'wasdSub', 'directionSub'] arrow or WASD keys held — "Miso.Subscription.Keyboard"
-- ['uriSub'] browser @popstate@ (back\/forward\/pushState) — "Miso.Subscription.History"
-- ['routerSub'] same, decoded via 'Miso.Router.Router' — "Miso.Subscription.History"
-- ['windowCoordsSub'] global window @pointermove@ — "Miso.Subscription.Window"
-- ['windowPointerMoveSub'] global window @pointermove@ — "Miso.Subscription.Window"
-- ['windowSubWithOptions'] any window event — "Miso.Subscription.Window"
-- ['onLineSub'] @online@ \/ @offline@ change — "Miso.Subscription.OnLine"
-- ['rAFSub'] every @requestAnimationFrame@ tick — "Miso.Subscription.RAF"
--
-- = History helpers
--
-- The "Miso.Subscription.History" module also exports imperative
-- navigation functions usable from within 'Miso.Effect.Effect':
--
-- @
-- update GoHome = 'Miso.Effect.io_' ('pushURI' ('Miso.Router.toURI' Home))
-- update GoBack = 'Miso.Effect.io_' 'back'
-- @
--
-- = See also
--
-- * "Miso.Effect" — 'Miso.Effect.Sub', 'Miso.Effect.Sink', 'Miso.Effect.mapSub'
-- * "Miso.Subscription.Util" — 'Miso.Subscription.Util.createSub' for custom subscriptions
-- * "Miso.Router" — 'Miso.Router.Router' typeclass used by 'routerSub'
----------------------------------------------------------------------------
module Miso.Subscription
  ( -- ** Mouse
    module Miso.Subscription.Mouse
    -- ** Keyboard
  , module Miso.Subscription.Keyboard
    -- ** History
  , module Miso.Subscription.History
    -- ** Window
  , module Miso.Subscription.Window
    -- ** OnLine
  , module Miso.Subscription.OnLine
    -- ** requestForAnimationFrame
  , module Miso.Subscription.RAF
  ) where
-----------------------------------------------------------------------------
import Miso.Subscription.Mouse
import Miso.Subscription.Keyboard
import Miso.Subscription.History
import Miso.Subscription.Window
import Miso.Subscription.OnLine
import Miso.Subscription.RAF
-----------------------------------------------------------------------------
