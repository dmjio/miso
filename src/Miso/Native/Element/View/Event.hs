-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.View.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.View.Event
  ( -- *** Events
    onTouchStart
  , onTouchStartWith
  , onTouchMove
  , onTouchMoveWith
  , onTouchEnd
  , onTouchEndWith
  , onTouchCancel
  , onTouchCancelWith
  , onTap
  , onTapMain
  , onTapWith
  , onTapMainWith
  , onTapMainModel
  , onLongPress
  , onLongPressWith
  , onLayoutChange
  , onLayoutChangeWith
  , onAppear
  , onAppearWith
  , onDisappear
  , onDisappearWith
  , onAnimationStart
  , onAnimationStartWith
  , onAnimationEnd
  , onAnimationEndWith
  , onAnimationCancel
  , onAnimationCancelWith
  , onAnimationIteration
  , onAnimationIterationWith
  , onTransitionStart
  , onTransitionStartWith
  , onTransitionEnd
  , onTransitionEndWith
  , onTransitionCancel
  , onTransitionCancelWith
    -- *** Types
  , TouchEvent (..)
  , AnimationEvent (..)
  , LayoutChangeDetailEvent (..)
  , UIAppearanceDetailEvent (..)
    -- *** Decoders
  , touchDecoder
  , animationDecoder
  , layoutChangeDetailDecoder
  , uiAppearanceDetailDecoder
    -- *** Event Map
  , viewEvents
  ) where
----------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 881
import Control.Applicative (liftA2)
#endif
-----------------------------------------------------------------------------
import qualified Data.Map as M
import           Miso.Event (on, onMain, Decoder(..), DecodeTarget(..), Events, emptyDecoder, Phase(BUBBLE))
import           Miso.JSON
import           Miso.String (MisoString)
import           Miso.Types (Attribute, EventHandler, DOMRef)
----------------------------------------------------------------------------
viewEvents :: Events
viewEvents = M.fromList
  [ ("touchstart", BUBBLE)
  , ("touchmove", BUBBLE)
  , ("touchend", BUBBLE)
  , ("touchcancel", BUBBLE)
  , ("tap", BUBBLE)
  , ("longpress", BUBBLE)
  , ("layoutchange", BUBBLE)
  , ("uiappear", BUBBLE)
  , ("uidisappear", BUBBLE)
  , ("animationstart", BUBBLE)
  , ("animationend", BUBBLE)
  , ("animationcancel", BUBBLE)
  , ("animationiteration", BUBBLE)
  , ("transitionstart", BUBBLE)
  , ("transitionend", BUBBLE)
  , ("transitioncancel", BUBBLE)
  ]
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/lynx-api/event/touch-event.html
data TouchEvent
  = TouchEvent
  { identifier :: Double
    -- ^ Unique identifier of the touch point, which remains
    -- unchanged during the same touch process
  , xy :: (Double, Double)
    -- ^ The horizontal / vertical position of the touch point in the
    -- coordinate system of the element actually touched
  , page :: (Double, Double)
    -- ^ The horizontal / vertical position of the touch point in the
    -- current LynxView coordinate system
  , client :: (Double, Double)
    -- ^ The horizontal / vertical position of the touch point in the
    -- current window coordinate system
  } deriving (Show, Eq)
----------------------------------------------------------------------------
-- | Touch decoder for use with events like 'onTap'
touchDecoder :: Decoder TouchEvent
touchDecoder = Decoder {..}
  where
    pair o x y = liftA2 (,) (o .: x) (o .: y)
    -- Lynx nests touch fields inside `changedTouches` / `touches` arrays (each a
    -- `Touch` with identifier/x/y/pageX/pageY/clientX/clientY); the browser-style
    -- flat root has no `identifier`. Read the first changed touch point.
    decodeAt = DecodeTarget ["changedTouches", "0"]
    decoder = withObject "touchDecoder" $ \o ->
       TouchEvent
        <$> o .: "identifier"
        <*> pair o "x" "y"
        <*> pair o "pageX" "pageY"
        <*> pair o "clientX" "clientY"
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/lynx-api/event/animation-event.html
data AnimationEvent
  = AnimationEvent
  { animationType :: AnimationType
    -- ^ The type of the animation. If it is a keyframe animation,
    -- this value is `keyframe-animation`; if it is a transition animation,
    -- this value is `transition-animation`.
  , animationName :: MisoString
    -- ^ The name of the animation. If it is a keyframe animation, it
    -- is the name of `@keyframes` in CSS; if it is a transition animation,
    -- it is the name of `transition-property` in CSS.
  , newAnimator :: Bool
    -- ^ Default value 'True'
  } deriving (Show, Eq)
----------------------------------------------------------------------------
data AnimationType
  = KeyFrameAnimation
  | TransitionAnimation
  deriving (Show, Eq)
----------------------------------------------------------------------------
instance FromJSON AnimationType where
  parseJSON = withText "animation-type" $ \case
    "keyframe-animation" -> pure KeyFrameAnimation
    "transition-animation" -> pure TransitionAnimation
    x -> typeMismatch "animation-type" (String x)
----------------------------------------------------------------------------
-- | Animation decoder for use with events like 'onAnimationStart'
animationDecoder :: Decoder AnimationEvent
animationDecoder = Decoder {..}
  where
    decodeAt = DecodeTarget mempty
    decoder = withObject "animationDecoder" $ \o ->
      AnimationEvent
        <$> o .: "animation_type"
        <*> o .: "animation_name"
        <*> o .: "new_animator"
-----------------------------------------------------------------------------
data LayoutChangeDetailEvent
  = LayoutChangeDetailEvent
  { layoutChangeDetailEventId :: MisoString
    -- ^ The id selector of the target.
  , layoutChangeDetailEventWidth :: Double
    -- ^ The width of the target.
  , layoutChangeDetailEventHeight :: Double
    -- ^ The height of the target.
  , layoutChangeDetailEventTop :: Double
    -- ^ The top of the target.
  , layoutChangeDetailEventRight :: Double
    -- ^ The right of the target.
  , layoutChangeDetailEventBottom :: Double
    -- ^ The bottom of the target.
  , layoutChangeDetailEventLeft :: Double
    -- ^ The left of the target.
  , layoutChangeDetailEventDataset :: Object
    -- ^ The dataset of the target.
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
layoutChangeDetailDecoder :: Decoder LayoutChangeDetailEvent
layoutChangeDetailDecoder = Decoder {..}
  where
    decodeAt = DecodeTarget mempty
    decoder = withObject "LayoutChangeDetailEvent" $ \o -> do
      d <- o .: "detail"
      LayoutChangeDetailEvent
        <$> d .: "id"
        <*> d .: "width"
        <*> d .: "height"
        <*> d .: "top"
        <*> d .: "right"
        <*> d .: "bottom"
        <*> d .: "left"
        <*> d .: "dataset"
-----------------------------------------------------------------------------
data UIAppearanceDetailEventType
  = UIAppear
  | UIDisappear
  deriving (Show, Eq)
----------------------------------------------------------------------------
instance FromJSON UIAppearanceDetailEventType where
  parseJSON = withText "UIAppearanceDetailEventType" $ \case
    "uiappear" -> pure UIAppear
    "uidisppear" -> pure UIDisappear
    x -> typeMismatch "UIAppearanceDetailEventType" (String x)
-----------------------------------------------------------------------------
data UIAppearanceDetailEvent
  = UIAppearanceDetailEvent
  { uiAppearanceDetailEventType :: UIAppearanceDetailEventType
  , uiAppearanceDetailEventExposureId :: MisoString
  , uiAppearanceDetailEventExposureScene :: MisoString
  , uiAppearanceDetailEventUniqueId :: MisoString
  , uiAppearanceDetailEventDataset :: Object
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
uiAppearanceDetailDecoder :: Decoder UIAppearanceDetailEvent
uiAppearanceDetailDecoder = Decoder {..}
  where
    decodeAt = DecodeTarget mempty
    decoder = withObject "UIAppearanceDetailEvent" $ \o -> do
      d <- o .: "detail"
      UIAppearanceDetailEvent
        <$> o .: "type"
        <*> d .: "exposure-id"
        <*> d .: "exposure-scene"
        <*> d .: "unique-id"
        <*> d .: "dataset"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#touchstart
--
-- It belongs to [touch event](https://lynxjs.org/api/lynx-api/event/touch-event.html),
-- which is triggered when the finger starts to touch the touch surface.
--
-- @
-- data Action = HandleTouch TouchEvent
--
-- view model = view_ [ onTouchStart HandleTouch ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleTouch TouchEvent {..}) = do
--   io_ (consoleLog "touch event received")
--
onTouchStart :: (TouchEvent -> action) -> Attribute model action
onTouchStart action = on "touchstart" touchDecoder (\x _ _ -> action x)
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#touchmove
--
-- It belongs to [touch event](https://lynxjs.org/api/lynx-api/event/touch-event.html),
-- which is triggered when the finger moves on the touch surface.
--
-- @
-- data Action = HandleTouch TouchEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = view_ [ onTouchMove HandleTouch ]
--
-- update :: Action -> Effect context props Model Action
-- update (HandleTouch TouchEvent {..}) = do
--   io_ (consoleLog "touch event received")
--
-- @
--
onTouchMove :: (TouchEvent -> action) -> Attribute model action
onTouchMove action = on "touchmove" touchDecoder (\x _ _ -> action x)
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#touchend
--
-- It belongs to [touch event](https://lynxjs.org/api/lynx-api/event/touch-event.html),
-- which is triggered when the finger leaves the touch surface.
--
-- @
-- data Action = HandleTouch TouchEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = view_ [ onTouchEnd HandleTouch ]
--
-- update :: Action -> Effect context props Model Action
-- update (HandleTouch TouchEvent {..}) = do
--   io_ (consoleLog "touch event received")
--
-- @
--
onTouchEnd :: (TouchEvent -> action) -> Attribute model action
onTouchEnd action = on "touchend" touchDecoder (\x _ _ -> action x)
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#touchcancel
--
-- It belongs to [touch event](https://lynxjs.org/api/lynx-api/event/touch-event.html),
-- which is triggered when the [touch event](https://lynxjs.org/api/lynx-api/event/touch-event.html),
-- is interrupted by the system or Lynx external gesture.
--
-- @
-- data Action = HandleTouch TouchEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = view_ [ onTouchCancel HandleTouch ]
--
-- update :: Action -> Effect context props Model Action
-- update (HandleTouch TouchEvent {..}) =
--   io_ (consoleLog "touch event received")
--
-- @
--
onTouchCancel :: (TouchEvent -> action) -> Attribute model action
onTouchCancel action = on "touchcancel" touchDecoder (\x _ _ -> action x)
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#tap
--
-- It belongs to [touch event](https://lynxjs.org/api/lynx-api/event/touch-event.html),
-- which is triggered when the finger clicks on the touch surface.
--
-- @
-- data Action = HandleTap
--
-- view :: context -> props -> Model -> View context Action
-- view model = view_ [ onTap HandleTap ]
--
-- update :: Action -> Effect context props Model Action
-- update HandleTap = io_ (consoleLog "touch event received")
--
-- @
--
onTap :: action -> Attribute model action
onTap action = on "tap" emptyDecoder (\() _ _ -> action)
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#tap
--
-- It belongs to [touch event](https://lynxjs.org/api/lynx-api/event/touch-event.html),
-- which is triggered when the finger clicks on the touch surface.
--
-- Unlike 'onTap', 'onTapMain' is necessary for handling tap events on the main thread (MTS).
--
-- @
-- data Action = HandleTap
--
-- view :: context -> props -> Model -> View context Action
-- view model = view_ [ event $ static (onTapMain HandleTap) ]
--
-- update :: Action -> Effect context props Model Action
-- update HandleTap = io_ (consoleLog "touch event received")
--
-- @
--
onTapMain :: action -> EventHandler model action
onTapMain action = onMain "tap" emptyDecoder (\() _ _ -> action)
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#tap
--
-- It belongs to [touch event](https://lynxjs.org/api/lynx-api/event/touch-event.html),
-- which is triggered when the finger clicks on the touch surface.
--
-- Unlike 'onTap', 'onTapMain' is necessary for handling tap events on the main thread (MTS).
--
-- @
-- data Action = HandleTap
--
-- view :: context -> props -> Model -> View context Action
-- view model = view_ [ event $ static (onTapMain HandleTap) ]
--
-- update :: Action -> Effect context props Model Action
-- update HandleTap = io_ (consoleLog "touch event received")
--
-- @
--
onTapMainWith :: (DOMRef -> action) -> EventHandler model action
onTapMainWith action = onMain "tap" emptyDecoder (\() _ -> action)
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#tap
--
-- It belongs to [touch event](https://lynxjs.org/api/lynx-api/event/touch-event.html),
-- which is triggered when the finger clicks on the touch surface.
--
-- Unlike 'onTap', 'onTapMain' is necessary for handling tap events on the main thread (MTS).
--
-- @
-- data Action = HandleTap
--
-- view :: context -> props -> Model -> View context Action
-- view model = view_ [ event $ static (onTapMain HandleTap) ]
--
-- update :: Action -> Effect context props Model Action
-- update HandleTap = io_ (consoleLog "touch event received")
--
-- @
--
onTapMainModel :: (model -> action) -> EventHandler model action
onTapMainModel action = onMain "tap" emptyDecoder (\() m _ -> action m)
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#longpress
--
-- It belongs to the touch event, which is triggered when the finger is long
-- pressed on the touch surface, and the interval between long press triggers is `500 ms`.
--
-- @
-- data Action = HandleTouch TouchEvent
--
-- view :: context -> props -> Model -> View context Action
-- view model = view_ [ onLongPress HandleTouch ]
--
-- update :: Action -> Effect context props Model Action
-- update (HandleTouch TouchEvent {..}) = io_ (consoleLog "touch event received")
--
-- @
--
onLongPress :: (TouchEvent -> action) -> Attribute model action
onLongPress action = on "longpress" touchDecoder (\x _ _ -> action x)
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#layoutchange
--
-- It belongs to a [custom event](https://lynxjs.org/api/lynx-api/event/custom-event.html), which is triggered when the target node layout
-- is completed, and returns the position information of the target node relative
-- to the LynxView viewport coordinate system.
--
-- @
-- data Action = HandleLayout LayoutChangeDetailEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = view_ [ onLayoutChange HandleLayout ]
--
-- update :: Action -> Effect context props Model Action
-- update (HandleLayout LayoutChangeDetailEvent {..}) =
--   io_ (consoleLog "layout changed")
-- @
--
onLayoutChange :: (LayoutChangeDetailEvent -> action) -> Attribute model action
onLayoutChange action = on "layoutchange" layoutChangeDetailDecoder (\x _ _ -> action x)
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#uiappear
--
-- It belongs to custom event, which is triggered when the target node appears on the screen.
--
-- @
-- data Action = HandleUI UIAppearanceDetailEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = view_ [ onAppear HandleUI ]
--
-- update :: Action -> Effect context props Model Action
-- update (HandleUI UIAppearanceDetailEvent {..}) = do
--   io_ (consoleLog "appearance detail event received")
-- @
--
onAppear :: (UIAppearanceDetailEvent -> action) -> Attribute model action
onAppear action = on "uiappear" uiAppearanceDetailDecoder (\x _ _ -> action x)
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#uidisappear
--
-- It belongs to custom event, which is triggered when the target node appears on the screen.
--
-- @
-- data Action = HandleUI UIAppearanceDetailEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = view_ [ onDisappear HandleUI ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleUI UIAppearanceDetailEvent {..}) = do
--   io_ (consoleLog "appearance detail event received")
-- @
--
onDisappear :: (UIAppearanceDetailEvent -> action) -> Attribute model action
onDisappear action = on "uidisappear" uiAppearanceDetailDecoder (\x _ _ -> action x)
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#animationstart
--
-- It belongs to [animation event](https://lynxjs.org/api/lynx-api/event/animation-event.html), which is triggered when the Animation animation starts.
--
-- @
-- data Action = HandleAnimation AnimationEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = view_ [ onAnimationStart HandleAnimation ]
--
-- update :: Action -> Effect context props Model Action
-- update (HandleAnimation AnimationEvent {..}) =
--   io_ (consoleLog "animation event received")
-- @
--
onAnimationStart :: (AnimationEvent -> action) -> Attribute model action
onAnimationStart action = on "animationstart" animationDecoder $ (\x _ _ -> action x)
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#animationend
--
-- It belongs to [animation event](https://lynxjs.org/api/lynx-api/event/animation-event.html), which is triggered when the Animation animation ends.
--
-- @
-- data Action = HandleAnimation AnimationEvent
--
-- view :: context -> props -> Model -> View context Action
-- view model = view_ [ onAnimationEnd HandleAnimation ]
--
-- update :: Action -> Effect context props Model Action
-- update (HandleAnimation AnimationEvent {..}) =
--   io_ (consoleLog "animation event received")
-- @
--
onAnimationEnd :: (AnimationEvent -> action) -> Attribute model action
onAnimationEnd action = on "animationend" animationDecoder (\x _ _ -> action x)
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#animationcancel
--
-- It belongs to [animation event](https://lynxjs.org/api/lynx-api/event/animation-event.html), which is triggered when the Animation animation cancels.
--
-- @
-- data Action = HandleAnimation AnimationEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = view_ [ onAnimationCancel HandleAnimation ]
--
-- update :: Action -> Effect context props Model Action
-- update (HandleAnimation AnimationEvent {..}) =
--   io_ (consoleLog "animation event received")
-- @
--
onAnimationCancel :: (AnimationEvent -> action) -> Attribute model action
onAnimationCancel action = on "animationcancel" animationDecoder (\x _ _ -> action x)
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#animationiteration
--
-- It belongs to [animation event](https://lynxjs.org/api/lynx-api/event/animation-event.html), which is triggered when the Animation animation iterates.
--
-- @
-- data Action = HandleAnimation AnimationEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = view_ [ onAnimationIteration HandleAnimation ]
--
-- update :: Action -> Effect context props Model Action
-- update (HandleAnimation AnimationEvent {..}) =
--   io_ (consoleLog "animation event received")
-- @
--
onAnimationIteration :: (AnimationEvent -> action) -> Attribute model action
onAnimationIteration action = on "animationiteration" animationDecoder (\x _ _ -> action x)
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#transitionstart
--
-- It belongs to [animation event](https://lynxjs.org/api/lynx-api/event/animation-event.html), which is triggered when the Transition animation starts.
--
-- @
-- data Action = HandleTransition AnimationEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = view_ [ onTransitionStart HandleTransition ]
--
-- update :: Action -> Effect context props Model Action
-- update (HandleTransition TransitionEvent {..}) =
--   io_ (consoleLog "transition event received")
-- @
--
onTransitionStart :: (AnimationEvent -> action) -> Attribute model action
onTransitionStart action = on "transitionstart" animationDecoder (\x _ _ -> action x)
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#transitionend
--
-- It belongs to [animation event](https://lynxjs.org/api/lynx-api/event/animation-event.html), which is triggered when the Transition animation ends.
--
-- @
-- data Action = HandleTransition AnimationEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = view_ [ onTransitionEnd HandleTransition ]
--
-- update :: Action -> Effect context props Model Action
-- update (HandleTransition TransitionEvent {..}) =
--   io_ (consoleLog "transition event received")
-- @
--
onTransitionEnd :: (AnimationEvent -> action) -> Attribute model action
onTransitionEnd action = on "transitionend" animationDecoder (\x _ _ -> action x)
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#transitioncancel
--
-- It belongs to [animation event](https://lynxjs.org/api/lynx-api/event/animation-event.html), which is triggered when the Transition animation cancels.
--
-- @
-- data Action = HandleTransition AnimationEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = view_ [ onTransitionCancel HandleTransition ]
--
-- update :: Action -> Effect context props Model Action
-- update (HandleTransition TransitionEvent {..}) =
--   io_ (consoleLog "transition event received")
-- @
--
onTransitionCancel :: (AnimationEvent -> action) -> Attribute model action
onTransitionCancel action = on "transitioncancel" animationDecoder (\x _ _ -> action x)
-----------------------------------------------------------------------------
-- | Like 'onTouchStart', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onTouchStartWith :: (TouchEvent -> DOMRef -> action) -> Attribute model action
onTouchStartWith action = on "touchstart" touchDecoder $ \t _ d -> action t d
-----------------------------------------------------------------------------
-- | Like 'onTouchMove', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onTouchMoveWith :: (TouchEvent -> DOMRef -> action) -> Attribute model action
onTouchMoveWith action = on "touchmove" touchDecoder $ \t _ d -> action t d
-----------------------------------------------------------------------------
-- | Like 'onTouchEnd', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onTouchEndWith :: (TouchEvent -> DOMRef -> action) -> Attribute model action
onTouchEndWith action = on "touchend" touchDecoder $ \t _ d -> action t d
-----------------------------------------------------------------------------
-- | Like 'onTouchCancel', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onTouchCancelWith :: (TouchEvent -> DOMRef -> action) -> Attribute model action
onTouchCancelWith action = on "touchcancel" touchDecoder $ \t _ d -> action t d
-----------------------------------------------------------------------------
-- | Like 'onTap', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onTapWith :: (DOMRef -> action) -> Attribute model action
onTapWith action = on "tap" emptyDecoder (\() _ ref -> action ref)
-----------------------------------------------------------------------------
-- | Like 'onLongPress', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onLongPressWith :: (TouchEvent -> DOMRef -> action) -> Attribute model action
onLongPressWith action = on "longpress" touchDecoder $ \t _ d -> action t d
-----------------------------------------------------------------------------
-- | Like 'onLayoutChange', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onLayoutChangeWith :: (LayoutChangeDetailEvent -> DOMRef -> action) -> Attribute model action
onLayoutChangeWith action = on "layoutchange" layoutChangeDetailDecoder $ \lcde _ domRef -> action lcde domRef
-----------------------------------------------------------------------------
-- | Like 'onAppear', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onAppearWith :: (UIAppearanceDetailEvent -> DOMRef -> action) -> Attribute model action
onAppearWith action = on "uiappear" uiAppearanceDetailDecoder $ \ui _ domRef -> action ui domRef
-----------------------------------------------------------------------------
-- | Like 'onDisappear', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onDisappearWith :: (UIAppearanceDetailEvent -> DOMRef -> action) -> Attribute model action
onDisappearWith action = on "uidisappear" uiAppearanceDetailDecoder $ \ui _ domRef -> action ui domRef
-----------------------------------------------------------------------------
-- | Like 'onAnimationStart', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onAnimationStartWith :: (AnimationEvent -> DOMRef -> action) -> Attribute model action
onAnimationStartWith action = on "animationstart" animationDecoder $ \ui _ domRef -> action ui domRef
-----------------------------------------------------------------------------
-- | Like 'onAnimationEnd', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onAnimationEndWith :: (AnimationEvent -> DOMRef -> action) -> Attribute model action
onAnimationEndWith action = on "animationend" animationDecoder $ \ui _ domRef -> action ui domRef
-----------------------------------------------------------------------------
-- | Like 'onAnimationCancel', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onAnimationCancelWith :: (AnimationEvent -> DOMRef -> action) -> Attribute model action
onAnimationCancelWith action = on "animationcancel" animationDecoder $ \ui _ domRef -> action ui domRef
-----------------------------------------------------------------------------
-- | Like 'onAnimationIteration', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onAnimationIterationWith :: (AnimationEvent -> DOMRef -> action) -> Attribute model action
onAnimationIterationWith action = on "animationiteration" animationDecoder $ \ui _ domRef -> action ui domRef
-----------------------------------------------------------------------------
-- | Like 'onTransitionStart', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onTransitionStartWith :: (AnimationEvent -> DOMRef -> action) -> Attribute model action
onTransitionStartWith action = on "transitionstart" animationDecoder $ \ui _ domRef -> action ui domRef
-----------------------------------------------------------------------------
-- | Like 'onTransitionEnd', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onTransitionEndWith :: (AnimationEvent -> DOMRef -> action) -> Attribute model action
onTransitionEndWith action = on "transitionend" animationDecoder $ \ui _ domRef -> action ui domRef
-----------------------------------------------------------------------------
-- | Like 'onTransitionCancel', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onTransitionCancelWith :: (AnimationEvent -> DOMRef -> action) -> Attribute model action
onTransitionCancelWith action = on "transitioncancel" animationDecoder $ \ui _ domRef -> action ui domRef
-----------------------------------------------------------------------------
