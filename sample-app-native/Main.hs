-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE DeriveAnyClass              #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE StaticPointers              #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE DerivingStrategies          #-}
-----------------------------------------------------------------------------
-- | Gallery: a scrolling showcase that exercises as many native Lynx elements,
-- properties, and events as we can. Every element wires up several handlers
-- that append to a running event log shown at the top of the screen — so
-- interacting with each widget demonstrates its events end-to-end on device.
--
-- All handlers are plain background (BTS) handlers wrapped in @event . static@,
-- as required by the dual-thread runtime.
module Main where
-----------------------------------------------------------------------------
import           Miso hiding (text_)
import           Miso.Native
import           Miso.Html.Property (id_, className, width_, height_)
import           Miso.Property (textProp)
import           Miso.Native.X.Element.Refresh.Method (finishRefresh)
import           Miso.Native.Element.Image.Method  (startAnimation, pauseAnimation, resumeAnimation, stopAnimation)
import           Miso.Native.MainThread (setStyleProperty)
import           Miso.Native.FFI (enableDebugging)
import           Control.Monad (when)
import           Control.Concurrent (threadDelay)
import qualified Miso.CSS as CSS
-----------------------------------------------------------------------------
-- Regular element events / properties
import qualified Miso.Native.Element.View.Event          as VE
import qualified Miso.Native.Element.Image.Event         as IE
import qualified Miso.Native.Element.Image.Property      as IP
import qualified Miso.Native.Element.ScrollView.Event    as SE
import qualified Miso.Native.Element.ScrollView.Property as SP
import qualified Miso.Native.Element.List.Event          as LE
import           Miso.Native.Element.List.Property     (ListOptions(..), ListType(..), ScrollOrientation(..), itemKey_)
import qualified Miso.Native.Element.Text.Event          as TE
import qualified Miso.Native.Element.Text.Property       as TP
-----------------------------------------------------------------------------
-- Extended (X) element constructors, events, properties
import           Miso.Native.X.Element
import qualified Miso.Native.X.Element.Input.Event        as InE
import           Miso.Native.X.Element.Input.Property   (placeholder_)
import qualified Miso.Native.X.Element.Textarea.Event     as TaE
import qualified Miso.Native.X.Element.Svg.Property       as SvP
import qualified Miso.Native.X.Element.Svg.Event          as SvE
import qualified Miso.Svg                                  as Svg
import qualified Miso.Svg.Property                         as SvgP
import qualified Miso.Native.X.Element.Viewpager.Event    as VpE
import qualified Miso.Native.X.Element.Viewpager.Property as VpP
import qualified Miso.Native.X.Element.Refresh.Event      as RE
import qualified Miso.Native.X.Element.Refresh.Property   as RP
import qualified Miso.Native.X.Element.Webview.Property   as WP
import qualified Miso.Native.X.Element.Webview.Event      as WE
import qualified Miso.Native.X.Element.Overlay.Event      as OE
import           Miso.Native.X.Element.Overlay.Property (visible_)
import qualified Miso.Native.X.Element.ScrollCoordinator.Event    as ScE
import qualified Miso.Native.X.Element.ScrollCoordinator.Property as ScP
-----------------------------------------------------------------------------
import           Miso.JSON (ToJSON, FromJSON)
import           GHC.Generics
-----------------------------------------------------------------------------
data Action
  = Log MisoString
  -- ^ append a message to the on-screen event log
  | ToggleLike
  -- ^ tap the heart: fill/unfill + (when liking) fire the burst
  | BurstOff
  -- ^ end the heart burst (scheduled shortly after a like)
  | ToggleOverlay
  -- ^ show/hide the overlay layer
  | RefreshStart
  -- ^ pull-to-refresh triggered: load rows, then finish
  | RefreshDone
  -- ^ finishRefresh acknowledged
  | ToggleTheme
  -- ^ flips the app-global 'Theme' context (see 'mountedComponentSection')
  | Pulse DOMRef
  -- ^ main-thread ('MTS') tap handler: scale-bounce the target element
  -- imperatively, no BTS round-trip (see 'mainThreadSection')
  | Fade DOMRef MisoString
  -- ^ main-thread ('MTS') scroll handler: fade the target element to the
  -- given opacity, computed from the scroll offset (see 'mainThreadSection')
  | MainThreadRead DOMRef Int
  -- ^ main-thread ('MTS') tap handler that /reads/ the hydrated model on the
  -- MTS (the @model@ param of 'onMain', same read path as 'onTapMainModel');
  -- carries the tapped element and a value read from that model, proving the
  -- BTS->MTS model-hydration read path (see 'mainThreadSection')
  | StorageInputChanged MisoString
  -- ^ the localStorage demo's input value changed
  | StorageSave
  -- ^ persist the current input value under 'storageKey'
  | StorageLoad
  -- ^ kick off an async read of 'storageKey'
  | StorageLoaded (Maybe MisoString)
  -- ^ 'StorageLoad' result: 'Nothing' if the key was never set (or cleared)
  | StorageClear
  -- ^ wipe all localStorage
  | AnimResume
  -- ^ resume the animated <image> (see 'animationSection')
  | AnimPause
  -- ^ pause the animated <image>, keeping loop-count
  | AnimStop
  -- ^ stop the animated <image>, resetting loop-count
  | AnimStart
  -- ^ restart the animated <image> via the deprecated @startAnimate@ method
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
-----------------------------------------------------------------------------
-- | App-global React-style context, exercised via 'nativeWithContext' /
-- 'modifyContext'. Read by 'mountedComponentSection' \/ 'badgeComponent' to
-- prove context propagates both to the top-level app and across the
-- dual-thread boundary into a statically-mounted child.
data Theme = Light | Dark
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
-----------------------------------------------------------------------------
data Model = Model
  { eventLog :: [MisoString]
    -- ^ most recent events, newest first (capped)
  , liked :: Bool
    -- ^ heart filled?
  , burst :: Bool
    -- ^ heart particle burst in flight?
  , overlayOpen :: Bool
    -- ^ overlay layer visibility
  , refreshRows :: Int
    -- ^ rows in the pull-to-refresh list (grows on refresh)
  , storageInput :: MisoString
    -- ^ pending value for the localStorage demo's <input>
  , storageLoaded :: Maybe MisoString
    -- ^ last value read back from localStorage ('Nothing' if unset/cleared)
  } deriving stock (Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
-----------------------------------------------------------------------------
main :: IO ()
main = do
  enableDebugging
  nativeWithContext (nativeEvents <> nativeXEvents) Dark
    (static (mountStatic_ galleryComponent))
-----------------------------------------------------------------------------
galleryComponent :: Component Theme () Model Action
galleryComponent = component (Model [] False False False 6 "" Nothing) updateModel viewModel
-----------------------------------------------------------------------------
updateModel :: Action -> Effect Theme () Model Action
updateModel = \case
  Log msg ->
    modify $ \m -> m { eventLog = take 10 (msg : eventLog m) }
  ToggleLike -> do
    nowLiked <- gets (not . liked)
    modify $ \m -> m { liked = nowLiked, burst = nowLiked
                     , eventLog = take 10 ((if nowLiked then "heart: like" else "heart: unlike") : eventLog m) }
    -- retract the particle burst a beat after a like (one-shot pop)
    when nowLiked $ io (threadDelay 650000 >> pure BurstOff)
  BurstOff ->
    modify $ \m -> m { burst = False }
  ToggleOverlay ->
    modify $ \m -> m { overlayOpen = not (overlayOpen m), eventLog = take 10 ("overlay: toggle" : eventLog m) }
  RefreshStart -> do
    modify $ \m -> m { refreshRows = refreshRows m + 3, eventLog = take 10 ("refresh: start (loading\8230)" : eventLog m) }
    finishRefresh "#refresh-demo" RefreshDone (const RefreshDone)
  RefreshDone ->
    modify $ \m -> m { eventLog = take 10 ("refresh: finished" : eventLog m) }
  ToggleTheme -> do
    modifyContext (\t -> if t == Dark then Light else Dark)
    modify $ \m -> m { eventLog = take 10 ("theme: toggle" : eventLog m) }
  Pulse domRef -> io_ $ do
    setStyleProperty domRef "transform" "scale(1.3)"
    threadDelay 150000
    setStyleProperty domRef "transform" "scale(1.0)"
  Fade domRef opacity ->
    io_ (setStyleProperty domRef "opacity" opacity)
  MainThreadRead domRef n ->
    -- Runs on the MTS (no BTS round-trip). 'n' was read from the MTS-hydrated
    -- model by the handler; reflect it visibly on the tapped element (opacity
    -- scaled to how full the 10-entry event log is) to prove the read path
    -- works, without routing through the console/error channel.
    io_ (setStyleProperty domRef "opacity" (ms (show (mtsReadOpacity n))))
  StorageInputChanged v ->
    modify $ \m -> m { storageInput = v }
  StorageSave -> do
    v <- gets storageInput
    io_ (setLocalStorage storageKey v)
    modify $ \m -> m { eventLog = take 10 ("storage: saved" : eventLog m) }
  StorageLoad ->
    io (StorageLoaded <$> getLocalStorage storageKey)
  StorageLoaded mv ->
    modify $ \m -> m
      { storageLoaded = mv
      , eventLog = take 10 (("storage: loaded " <> maybe "(nothing)" id mv) : eventLog m)
      }
  StorageClear -> do
    io_ clearLocalStorage
    modify $ \m -> m { storageLoaded = Nothing, eventLog = take 10 ("storage: cleared" : eventLog m) }
  -- 'invokeExec'-backed <image> animation methods (see 'animationSection'). Each
  -- dispatches a UI method to the element selected by id and logs the outcome.
  AnimResume -> resumeAnimation animId (Log "anim: resumed") (Log . ("anim: error " <>))
  AnimPause  -> pauseAnimation  animId (Log "anim: paused")  (Log . ("anim: error " <>))
  AnimStop   -> stopAnimation   animId (Log "anim: stopped") (Log . ("anim: error " <>))
  AnimStart  -> startAnimation  animId (Log "anim: started (startAnimate)") (Log . ("anim: error " <>))
-----------------------------------------------------------------------------
-- | Key used by 'storageSection' with 'getLocalStorage' \/
-- 'setLocalStorage' \/ 'clearLocalStorage' (see "Miso.Storage").
storageKey :: MisoString
storageKey = "gallery-demo-key"
-----------------------------------------------------------------------------
-- | @id@ selector for the animated \<image\> exercised by 'animationSection'.
-- Passed to the 'invokeExec'-backed 'startAnimation' \/ 'pauseAnimation' \/
-- 'resumeAnimation' \/ 'stopAnimation' methods.
animId :: MisoString
animId = "#anim-gif"
-----------------------------------------------------------------------------
viewModel :: Theme -> () -> Model -> View Theme Model Action
viewModel theme _ Model{..} = view_
  [ CSS.style_
    [ CSS.height "100vh"
    , CSS.width "100%"
    , CSS.display "flex"
    , CSS.flexDirection "column"
    ]
  ]
  [ logView eventLog
  , scrollView_
    [ SP.scrollOrientation_ "vertical"
    , SP.enableScroll_ True
    , SP.scrollBarEnable_ True
    , SE.onScroll (\_ -> Log "gallery scroll")
    , CSS.style_
      [ CSS.flexGrow 1
      , CSS.width "100%"
      , CSS.display "flex"
      , CSS.flexDirection "column"
      ]
    ]
    [ classSection
    , viewSection
    , heartSection liked burst
    , textSection
    , imageSection
    , animationSection
    , nestedScrollSection
    , scrollCoordinatorSection
    , listSection
    , inputSection
    , textareaSection
    , viewpagerSection
    , refreshSection refreshRows
    , svgSection
    , blurViewSection
    , webviewSection
    , overlaySection overlayOpen
    , mainThreadSection
    , mountedComponentSection theme (length eventLog)
    , storageSection storageInput storageLoaded
    ]
  ]
-----------------------------------------------------------------------------
-- | Explicit line-height for every text node. Without it Lynx measures text
-- with @lineHeight:nan@ (the @LynxTextRender ... lineHeight:nan@ log lines and
-- the CoreGraphics NaN warnings). Unitless scales with font-size.
txtLine :: CSS.Style
txtLine = CSS.lineHeight "1.4"
-----------------------------------------------------------------------------
-- | Sticky header showing the latest events.
logView :: [MisoString] -> View context Model Action
logView msgs = view_
  [ CSS.style_
    [ CSS.height "120px"
    , CSS.width "100%"
    , CSS.flexShrink 0
    , CSS.display "flex"
    , CSS.flexDirection "column"
    , CSS.padding "8px"
    , CSS.backgroundColor CSS.black
    ]
  ]
  [ text_ [ CSS.style_ [ CSS.fontSize "14px", CSS.color CSS.lime, txtLine ] ]
      [ text (if null msgs then "\9679 interact below \8212 events appear here" else "\9679 events:") ]
  , text_ [ CSS.style_ [ CSS.fontSize "12px", CSS.color CSS.white, txtLine ] ]
      [ text (ms (unlinesTake msgs)) ]
  ]
  where
    unlinesTake = foldr (\a b -> a <> "\n" <> b) "" . take 5
-----------------------------------------------------------------------------
-- | Wrap a demo in a labelled bordered card.
section :: MisoString -> [View context Model Action] -> View context Model Action
section label kids = view_
  [ CSS.style_
    [ CSS.width "100%"
    , CSS.flexShrink 0
    , CSS.display "flex"
    , CSS.flexDirection "column"
    , CSS.padding "10px"
    , CSS.marginBottom "1px"
    , CSS.backgroundColor (CSS.RGB 245 245 245)
    ]
  ]
  ( text_ [ CSS.style_ [ CSS.fontSize "16px", CSS.color CSS.darkblue, CSS.marginBottom "6px", txtLine ] ]
      [ text label ]
  : kids
  )
-----------------------------------------------------------------------------
-- | Demonstrates a class-based stylesheet compiled into the Lynx bundle. The
-- @.foo@ rule lives in @sample-app-native/styles.css@, which the @bundle-lynx@
-- justfile recipe imports into the rspeedy entry. On device it resolves via
-- @__AddClass@ against the global cssId 0 stylesheet the runtime scopes the
-- page to. If @.foo@ applies you get a crimson, padded, rounded card; if the
-- stylesheet wiring is broken you get plain unstyled text.
classSection :: View context Model Action
classSection = section "className \8212 .foo rule from styles.css"
  -- className is on the text element itself: Lynx does not cascade `color`
  -- from <view> to child <text>, and there is no inline color to override it,
  -- so crimson text = the class resolved; default/black text = it did not.
  [ text_ [ className "foo", CSS.style_ [ txtLine ] ]
      [ text "this text is crimson via .foo in styles.css" ]
  ]
-----------------------------------------------------------------------------
-- | <view> — gestures. Each gesture lives on its own element on purpose: Lynx
-- arbitrates tap vs longpress on a single element (a bound @longpress@ consumes
-- @tap@ — "Lynx Send Tap Event failed, longpress consumed"), which only shows
-- up on a physical device (a simulator "tap" is instantaneous and never engages
-- the longpress path). So don't stack tap + longpress on the same node.
viewSection :: View context Model Action
viewSection = section "<view> \8212 gestures (each on its own element)"
  [ gestureBox CSS.steelblue "tap me"
      [ VE.onTap (Log "view: tap")
      , VE.onLayoutChange (\_ -> Log "view: layoutchange")
      ]
  , gestureBox CSS.seagreen "long-press me"
      [ VE.onLongPress (\_ -> Log "view: longpress") ]
  , gestureBox CSS.slateblue "touch me (start/end)"
      [ VE.onTouchStart (\_ -> Log "view: touchstart")
      , VE.onTouchEnd (\_ -> Log "view: touchend")
      ]
  ]
  where
    gestureBox bg lbl handlers = view_
      ( handlers ++
        [ CSS.style_
          [ CSS.height "48px", CSS.marginBottom "6px"
          , CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "center"
          , CSS.backgroundColor bg
          ]
        ]
      )
      [ text_ [ CSS.style_ [ CSS.color CSS.white, txtLine ] ] [ text lbl ] ]
-----------------------------------------------------------------------------
-- | A Rednote/Xiaohongshu-style "like" animation, built entirely from CSS
-- transitions. Tapping the heart toggles @liked@: it fills crimson and springs
-- up (a spring cubic-bezier on @transform@). Liking also flips @burst@ on,
-- which flings a ring of little hearts outward+up from the centre; the update
-- schedules 'BurstOff' ~0.65s later so they retract — a one-shot pop.
heartSection :: Bool -> Bool -> View context Model Action
heartSection isLiked isBurst = section "animation \8212 tap the heart (Rednote-style like)"
  [ view_
    [ CSS.style_
      [ CSS.width "100%", CSS.height "150px", CSS.position "relative"
      , CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "center"
      ]
    ]
    ( [ particle isBurst off | off <- burstOffsets ] ++
      [ view_
        [ VE.onTap ToggleLike
        , CSS.style_
          [ CSS.width "90px", CSS.height "90px"
          , CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "center"
          ]
        ]
        [ text_
          [ CSS.style_
            [ CSS.fontSize "60px"
            , CSS.color (if isLiked then CSS.crimson else CSS.silver)
            , CSS.transition "transform 0.35s cubic-bezier(0.2,1.7,0.4,1), color 0.3s ease"
            , CSS.transform (if isLiked then "scale(1.15)" else "scale(1.0)")
            , txtLine
            ]
          ]
          [ text "\9829" ]
        ]
      ]
    )
  ]
  where
    -- spread targets for the little hearts (px offsets from centre)
    burstOffsets :: [(MisoString, MisoString)]
    burstOffsets =
      [ ("-46","-58"), ("46","-58"), ("-66","-8"), ("66","-8")
      , ("0","-82"), ("-22","-40"), ("22","-40") ]
    particle on_ (dx, dy) = text_
      [ CSS.style_
        [ CSS.position "absolute", CSS.left "50%", CSS.top "50%"
        , CSS.fontSize "20px", CSS.color CSS.crimson
        , CSS.transition "transform 0.6s ease-out, opacity 0.6s ease-out"
        , CSS.opacity (if on_ then 0.95 else 0)
        , CSS.transform (if on_
            then "translate(-50%,-50%) translate(" <> dx <> "px," <> dy <> "px) scale(1)"
            else "translate(-50%,-50%) scale(0.2)")
        , txtLine
        ]
      ]
      [ text "\9829" ]
-----------------------------------------------------------------------------
-- | <text> — layout + selection, with selectable text.
textSection :: View context Model Action
textSection = section "<text> \8212 layout / selection"
  [ text_
    [ TP.textMaxLine_ 3
    , TP.textSelection_ True
    , TE.onLayout (\_ -> Log "text: layout")
    , TE.onSelectionChange (\_ -> Log "text: selectionchange")
    , CSS.style_ [ CSS.fontSize "15px", CSS.color CSS.black, txtLine ]
    ]
    [ text "Selectable text. Long-press to select and watch the selectionchange event fire." ]
  ]
-----------------------------------------------------------------------------
-- | <image> — load / error, with a resize mode.
imageSection :: View context Model Action
imageSection = section "<image> \8212 load / error"
  [ image_ "https://picsum.photos/300/120"
    [ IP.mode_ "aspectFill"
    , IE.onLoad (\_ -> Log "image: load")
    , IE.onError (\_ -> Log "image: error")
    , CSS.style_ [ CSS.width "100%", CSS.height "120px" ]
    ]
  ]
-----------------------------------------------------------------------------
-- | <image> animation methods — exercises the 'invokeExec'-backed
-- 'resumeAnimation' \/ 'pauseAnimation' \/ 'stopAnimation' \/ 'startAnimation'
-- UI methods (Lynx @SelectorQuery.invoke@) against an animated GIF. The GIF is
-- loaded with @autoPlay_ False@ so "resume" has a visible effect; each button
-- dispatches its method to the element selected by 'animId' and logs the result.
-- 'startAnimation' maps to Lynx's deprecated @startAnimate@; the others are the
-- current API.
animationSection :: View context Model Action
animationSection = section "<image> animation \8212 startAnimate / pause / resume / stop (invokeExec)"
  [ image_ "https://upload.wikimedia.org/wikipedia/commons/2/2c/Rotating_earth_%28large%29.gif"
    [ id_ "anim-gif"
    , IP.mode_ "aspectFit"
    , IP.autoPlay_ False
    , IP.loopCount_ 0
    , IE.onLoad (\_ -> Log "anim: image loaded")
    , IE.onError (\_ -> Log "anim: image error")
    , CSS.style_ [ CSS.width "100%", CSS.height "140px" ]
    ]
  , view_
    [ CSS.style_ [ CSS.display "flex", CSS.flexDirection "row", CSS.marginTop "6px" ] ]
    [ animButton CSS.seagreen  "resume" AnimResume
    , animButton CSS.goldenrod "pause"  AnimPause
    , animButton CSS.crimson   "stop"   AnimStop
    , animButton CSS.slategray "start*" AnimStart
    ]
  ]
  where
    animButton bg lbl action = view_
      [ VE.onTap action
      , CSS.style_
        [ CSS.flexGrow 1, CSS.height "40px", CSS.marginRight "6px"
        , CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "center"
        , CSS.backgroundColor bg
        ]
      ]
      [ text_ [ CSS.style_ [ CSS.color CSS.white, txtLine ] ] [ text lbl ] ]
-----------------------------------------------------------------------------
-- | <scroll-view> — nested horizontal scroll with edge + size events.
nestedScrollSection :: View context Model Action
nestedScrollSection = section "<scroll-view> \8212 horizontal, scroll edges"
  [ scrollView_
    [ SP.scrollOrientation_ "horizontal"
    , SP.enableScroll_ True
    , SP.bounces_ True
    , SE.onScrollToUpper (\_ -> Log "scroll-view: to-upper")
    , SE.onScrollToLower (\_ -> Log "scroll-view: to-lower")
    , SE.onScrollEnd (\_ -> Log "scroll-view: scroll-end")
    , SE.onContentSizeChanged (\_ -> Log "scroll-view: content-size-changed")
    , CSS.style_ [ CSS.width "100%", CSS.height "60px", CSS.display "flex", CSS.flexDirection "row" ]
    ]
    [ swatch i | i <- [1 .. 8 :: Int] ]
  ]
  where
    swatch i = view_
      [ CSS.style_
        [ CSS.width "80px", CSS.height "60px", CSS.flexShrink 0
        , CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "center"
        , CSS.backgroundColor (if even i then CSS.coral else CSS.teal)
        ]
      ]
      [ text_ [ CSS.style_ [ CSS.color CSS.white, txtLine ] ] [ text (ms (show i)) ] ]
-----------------------------------------------------------------------------
-- | <scroll-coordinator> — a foldview: a collapsing header coordinated with a
-- nested scroll. It requires a @scroll-coordinator-header@ and a
-- @scroll-coordinator-slot@ as children (a bare view/scroll-view is rejected
-- with error 990100). The slot holds the scrollable content; scrolling it folds
-- the header away, and the coordinator emits `offset` as it folds.
scrollCoordinatorSection :: View context Model Action
scrollCoordinatorSection = section "<scroll-coordinator> \8212 collapsing header + nested scroll"
  [ scrollCoordinator_
    [ ScP.enableScroll_ True
    , ScP.headerOverSlot_ True
    , ScE.onOffset (\_ -> Log "scroll-coordinator: offset")
    , CSS.style_ [ CSS.width "100%", CSS.height "200px", CSS.display "flex", CSS.flexDirection "column" ]
    ]
    [ scrollCoordinatorHeader_
      [ CSS.style_
        [ CSS.width "100%", CSS.height "80px", CSS.flexShrink 0
        , CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "center"
        , CSS.backgroundColor CSS.darkslategray
        ]
      ]
      [ text_ [ CSS.style_ [ CSS.color CSS.white, txtLine ] ] [ text "collapsing header \8212 scroll to fold" ] ]
    , scrollCoordinatorSlot_
      [ CSS.style_ [ CSS.width "100%", CSS.flexGrow 1, CSS.display "flex", CSS.flexDirection "column" ] ]
      [ scrollView_
        [ SP.scrollOrientation_ "vertical"
        , SP.enableScroll_ True
        , CSS.style_ [ CSS.width "100%", CSS.height "120px", CSS.display "flex", CSS.flexDirection "column" ]
        ]
        [ view_
          [ CSS.style_
            [ CSS.width "100%", CSS.height "36px", CSS.flexShrink 0
            , CSS.display "flex", CSS.alignItems "center", CSS.padding "8px"
            , CSS.backgroundColor (if even i then CSS.white else CSS.gainsboro)
            ]
          ]
          [ text_ [ CSS.style_ [ txtLine ] ] [ text (ms ("coord row " <> show i)) ] ]
        | i <- [1 .. 12 :: Int]
        ]
      ]
    ]
  ]
-----------------------------------------------------------------------------
-- | <list> — recycler with item-keys, snap, layout-complete.
listSection :: View context Model Action
listSection = section "<list> \8212 recycler, snap, layout-complete"
  [ list_ (ListOptions Single 1 Vertical)
    [ LE.onScroll (\_ -> Log "list: scroll")
    , LE.onScrollToLower (\_ -> Log "list: to-lower")
    , LE.onSnap (\_ -> Log "list: snap")
    , LE.onLayoutComplete (\_ -> Log "list: layout-complete")
    , CSS.style_ [ CSS.width "100%", CSS.height "140px" ]
    ]
    [ listItem_
      [ itemKey_ (ms ("row-" <> show i))
      , CSS.style_
        [ CSS.width "100%", CSS.height "44px"
        , CSS.display "flex", CSS.alignItems "center", CSS.padding "8px"
        , CSS.backgroundColor (if even i then CSS.white else CSS.gainsboro)
        ]
      ]
      [ text_ [ CSS.style_ [ txtLine ] ] [ text (ms ("list row " <> show i)) ] ]
    | i <- [1 .. 20 :: Int]
    ]
  ]
-----------------------------------------------------------------------------
-- | <input> — the full input event set.
inputSection :: View context Model Action
inputSection = section "<input> \8212 input / focus / blur / confirm"
  [ input_
    [ placeholder_ "type here\8230"
    , InE.onInput (\e -> Log ("input: " <> InE.inputValue e))
    , InE.onFocus (\_ -> Log "input: focus")
    , InE.onBlur (\_ -> Log "input: blur")
    , InE.onConfirm (\_ -> Log "input: confirm")
    , CSS.style_
      [ CSS.height "44px", CSS.width "100%", CSS.fontSize "16px"
      , CSS.backgroundColor CSS.white, CSS.padding "8px"
      ]
    ]
  ]
-----------------------------------------------------------------------------
-- | <textarea> — multiline input.
textareaSection :: View context Model Action
textareaSection = section "<textarea> \8212 multiline input / focus / blur"
  [ textarea_
    [ placeholder_ "multiline\8230"
    , TaE.onInput (\_ -> Log "textarea: input")
    , TaE.onFocus (\_ -> Log "textarea: focus")
    , TaE.onBlur (\_ -> Log "textarea: blur")
    , CSS.style_
      [ CSS.height "70px", CSS.width "100%", CSS.fontSize "15px"
      , CSS.backgroundColor CSS.white, CSS.padding "8px"
      ]
    ]
  ]
-----------------------------------------------------------------------------
-- | <viewpager> — paged container with change / offset events.
viewpagerSection :: View context Model Action
viewpagerSection = section "<viewpager> \8212 paged, change / offset"
  [ viewpager_
    [ VpP.bounces_ True
    , VpP.initialSelectIndex_ 0
    , VpE.onChange (\_ -> Log "viewpager: change")
    , VpE.onOffsetChange (\_ -> Log "viewpager: offset-change")
    , VpE.onWillChange (\_ -> Log "viewpager: will-change")
    , CSS.style_ [ CSS.width "100%", CSS.height "100px" ]
    ]
    [ viewpagerItem_
      [ CSS.style_
        [ CSS.width "100%", CSS.height "100px"
        , CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "center"
        , CSS.backgroundColor (if even i then CSS.mediumpurple else CSS.orange)
        ]
      ]
      [ text_ [ CSS.style_ [ CSS.color CSS.white, CSS.fontSize "20px", txtLine ] ]
          [ text (ms ("page " <> show i)) ] ]
    | i <- [1 .. 3 :: Int]
    ]
  ]
-----------------------------------------------------------------------------
-- | <refresh> — pull-to-refresh. The native refresh (MJRefresh) attaches its
-- pull gesture to the first *UIScrollView descendant*, so the content MUST be a
-- <scroll-view> (the web preview says otherwise, but native needs it). Pull
-- down past the top to reveal the header and fire `startrefresh`; the update
-- then adds rows and calls `finishRefresh` (selected by the element `id`).
refreshSection :: Int -> View context Model Action
refreshSection rows = section "<refresh> \8212 pull down from the top to load rows"
  [ refresh_
    [ id_ "refresh-demo"
    , RP.enableRefresh_ True
    , RE.onStartRefresh (\_ -> RefreshStart)
    , RE.onRefreshStateChange (\_ -> Log "refresh: state-change")
    , CSS.style_ [ CSS.width "100%", CSS.height "180px" ]
    ]
    [ refreshHeader_
      [ CSS.style_
        [ CSS.width "100%", CSS.height "44px", CSS.flexShrink 0
        , CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "center"
        , CSS.backgroundColor CSS.gainsboro
        ]
      ]
      [ text_ [ CSS.style_ [ txtLine ] ] [ text "\8635 release to refresh" ] ]
    , scrollView_
      [ SP.scrollOrientation_ "vertical"
      , SP.enableScroll_ True
      , CSS.style_ [ CSS.width "100%", CSS.height "136px", CSS.display "flex", CSS.flexDirection "column" ]
      ]
      [ view_
        [ CSS.style_
          [ CSS.width "100%", CSS.height "36px", CSS.flexShrink 0
          , CSS.display "flex", CSS.alignItems "center", CSS.padding "8px"
          , CSS.backgroundColor (if even i then CSS.white else CSS.gainsboro)
          ]
        ]
        [ text_ [ CSS.style_ [ txtLine ] ] [ text (ms ("refresh row " <> show i)) ] ]
      | i <- [1 .. rows]
      ]
    ]
  ]
-----------------------------------------------------------------------------
-- | <svg> — inline vector content, built with the "Miso.Svg" DSL rather than a
-- hand-written XML string. `content_` renders the 'View' to an SVG string (via
-- `toHtml`); its @ByteString -> MisoString@ step relies on the JS @TextDecoder@
-- global, polyfilled for the Lynx runtime in @ts/miso-native.ts@. `toHtml`
-- doesn't emit @xmlns@, so we add it explicitly to keep the content a
-- well-formed standalone SVG document.
svgSection :: View context Model Action
svgSection = section "<svg> \8212 inline content via Miso.Svg DSL / load"
  [ svg_
    [ SvP.content_ svgArt
    , SvE.onLoad (Log "svg: load")
    , CSS.style_ [ CSS.width "100px", CSS.height "100px" ]
    ]
    []
  ]
  where
    svgArt :: View context Model Action
    svgArt = Svg.svg_
      [ SvgP.viewBox_ "0 0 100 100"
      , textProp "xmlns" "http://www.w3.org/2000/svg"
      ]
      [ Svg.circle_ [ SvgP.cx_ "50", SvgP.cy_ "50", SvgP.r_ "40", SvgP.fill_ "tomato" ]
      , Svg.rect_ [ SvgP.x_ "20", SvgP.y_ "20", width_ "20", height_ "20", SvgP.fill_ "royalblue" ]
      ]
-----------------------------------------------------------------------------
-- | <blur-view> — material blur backdrop.
blurViewSection :: View context Model Action
blurViewSection = section "<blur-view> \8212 material backdrop (blurs the image behind)"
  [ view_
    [ CSS.style_ [ CSS.width "100%", CSS.height "100px", CSS.position "relative" ] ]
    -- an image fills the box; the blur-view is positioned over its lower half so
    -- the blur is actually visible (a blur-view with nothing behind shows nothing)
    [ image_ "https://picsum.photos/400/200"
      [ IP.mode_ "aspectFill"
      , CSS.style_ [ CSS.position "absolute", CSS.width "100%", CSS.height "100%" ]
      ]
    , blurView_
      [ CSS.style_
        [ CSS.position "absolute", CSS.left "0", CSS.bottom "0"
        , CSS.width "100%", CSS.height "50px"
        , CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "center"
        ]
      ]
      [ text_ [ CSS.style_ [ CSS.color CSS.white, txtLine ] ] [ text "the image behind is blurred" ] ]
    ]
  ]
-----------------------------------------------------------------------------
-- | <webview> — embedded web page.
webviewSection :: View context Model Action
webviewSection = section "<webview> \8212 embedded page, load / error"
  [ webview_
    [ WP.src_ "https://haskell-miso.org"
    , WP.bounces_ True
    , WE.onLoad (Log "webview: load")
    , WE.onError (\_ -> Log "webview: error")
    , WE.onLocationChange (\_ -> Log "webview: location-change")
    , CSS.style_ [ CSS.width "100%", CSS.height "160px" ]
    ]
  ]
-----------------------------------------------------------------------------
-- | <overlay> — a full-screen layer above the page, toggled by a button.
-- (An always-visible overlay would cover the whole gallery, so we gate it on
-- @visible_@ and let the user open/close it.)
overlaySection :: Bool -> View context Model Action
overlaySection open = section "<overlay> \8212 independent layer (tap to open)"
  [ view_
    [ VE.onTap ToggleOverlay
    , CSS.style_
      [ CSS.height "44px", CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "center"
      , CSS.backgroundColor CSS.indigo
      ]
    ]
    [ text_ [ CSS.style_ [ CSS.color CSS.white, txtLine ] ] [ text "open overlay" ] ]
  , overlay_
    [ visible_ open
    , OE.onShowOverlay (Log "overlay: show")
    , OE.onDismissOverlay (Log "overlay: dismiss")
    , OE.onOverlayTouch (\_ -> Log "overlay: touch")
    , OE.onRequestClose ToggleOverlay
    ]
    [ view_
      [ VE.onTap ToggleOverlay
      , CSS.style_
        [ CSS.width "100%", CSS.height "100%"
        , CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "center"
        , CSS.backgroundColor (CSS.RGBA 0 0 0 0.7)
        ]
      ]
      [ text_ [ CSS.style_ [ CSS.color CSS.white, CSS.fontSize "20px", txtLine ] ]
          [ text "overlay content \8212 tap to dismiss" ] ]
    ]
  ]
-----------------------------------------------------------------------------
-- | Main-thread ('MTS') event handlers — dispatched via 'event' \/ 'static',
-- run imperatively with no VDOM diff and no BTS round-trip. 'Pulse' fires from
-- a tap ('onTapMainWith'); 'Fade' fires from scrolling ('onScrollMainWith')
-- and fades the scroll-view itself based on scroll offset. Each mutates only
-- a property the declarative 'CSS.style_' below never sets ("transform" /
-- "opacity"), per the single-owner discipline in "Miso.Native.MainThread".
mainThreadSection :: View context Model Action
mainThreadSection = section "main-thread (MTS) events \8212 gesture + scroll, no BTS round-trip"
  [ view_
    [ event (static (VE.onTapMainWith Pulse))
    , CSS.style_
      [ CSS.width "100%", CSS.height "48px"
      , CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "center"
      , CSS.backgroundColor CSS.darkorange
      , CSS.transition "transform 0.15s ease"
      ]
    ]
    [ text_ [ CSS.style_ [ CSS.color CSS.white, txtLine ] ]
        [ text "tap to pulse (main-thread, no round-trip)" ] ]
  , view_
    [ event (static (onMain "tap" emptyDecoder (\() m domRef -> MainThreadRead domRef (length (eventLog m)))))
    , CSS.style_
      [ CSS.width "100%", CSS.height "48px", CSS.marginTop "6px"
      , CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "center"
      , CSS.backgroundColor CSS.mediumpurple
      , CSS.transition "opacity 0.2s ease"
      ]
    ]
    [ text_ [ CSS.style_ [ CSS.color CSS.white, txtLine ] ]
        [ text "tap: read model on main-thread (opacity = event-log fill)" ] ]
  , scrollView_
    [ event (static (SE.onScrollMainWith (\SE.ScrollEvent{..} _ domRef -> Fade domRef (opacityFor scrollTop))))
    , SP.scrollOrientation_ "vertical"
    , SP.enableScroll_ True
    , CSS.style_
      [ CSS.width "100%", CSS.height "90px", CSS.display "flex", CSS.flexDirection "column"
      , CSS.backgroundColor CSS.gold
      ]
    ]
    [ view_
      [ CSS.style_
        [ CSS.width "100%", CSS.height "36px", CSS.flexShrink 0
        , CSS.display "flex", CSS.alignItems "center", CSS.padding "8px"
        , CSS.backgroundColor (if even i then CSS.white else CSS.gainsboro)
        ]
      ]
      [ text_ [ CSS.style_ [ txtLine ] ] [ text (ms ("scroll to fade row " <> show i)) ] ]
    | i <- [1 .. 8 :: Int]
    ]
  ]
-----------------------------------------------------------------------------
-- | Maps a scroll-view's @scrollTop@ to an opacity string, clamped to
-- [0.2, 1.0] over the first 150px of scroll. Used by 'mainThreadSection'.
opacityFor :: Double -> MisoString
opacityFor y = ms (show (max 0.2 (1 - min 1 (y / 150))))
-----------------------------------------------------------------------------
-- | Maps the event-log entry count (0..10) read on the MTS to an opacity in
-- [0.35, 1.0]. Used by 'mainThreadSection' to make the 'MainThreadRead' model
-- read visible on the tapped element.
mtsReadOpacity :: Int -> Double
mtsReadOpacity n = max 0.35 (min 1 (fromIntegral n / 10))
-----------------------------------------------------------------------------
-- | Props for the statically-mounted child below: derived from the parent
-- 'Model' and re-sent on every parent update, so a growing 'badgeEventCount'
-- proves props update across the dual-thread boundary, not just at the
-- initial mount.
data BadgeProps = BadgeProps
  { badgeEventCount :: Int
  } deriving stock (Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
-----------------------------------------------------------------------------
-- | Child-local state: lives and updates entirely inside 'badgeComponent',
-- independent of the parent's 'Model'.
data BadgeModel = BadgeModel
  { badgeTaps :: Int
  } deriving stock (Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
-----------------------------------------------------------------------------
data BadgeAction
  = BadgeTap
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
-----------------------------------------------------------------------------
-- | Mounted statically via 'vcomp' from 'mountedComponentSection' — this is
-- the only place in the gallery that mounts a component /outside/ the
-- top-level app. Opts into 'useContext' so it re-renders when the parent
-- flips 'Theme' via 'ToggleTheme'.
badgeComponent :: Component Theme BadgeProps BadgeModel BadgeAction
badgeComponent = (component (BadgeModel 0) badgeUpdate badgeView) { useContext = True }
-----------------------------------------------------------------------------
badgeUpdate :: BadgeAction -> Effect Theme BadgeProps BadgeModel BadgeAction
badgeUpdate BadgeTap = modify $ \m -> m { badgeTaps = badgeTaps m + 1 }
-----------------------------------------------------------------------------
badgeView :: Theme -> BadgeProps -> BadgeModel -> View Theme BadgeModel BadgeAction
badgeView theme BadgeProps{..} BadgeModel{..} = view_
  [ VE.onTap BadgeTap
  , CSS.style_
    [ CSS.width "100%", CSS.height "56px"
    , CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "center"
    , CSS.backgroundColor (if theme == Dark then CSS.midnightblue else CSS.lightyellow)
    ]
  ]
  [ text_
    [ CSS.style_ [ CSS.color (if theme == Dark then CSS.white else CSS.black), txtLine ] ]
    [ text (ms ("parent events: " <> show badgeEventCount <> " \183 child taps: " <> show badgeTaps)) ]
  ]
-----------------------------------------------------------------------------
-- | Mounts 'badgeComponent' statically with props derived from the parent
-- model, and a "toggle theme" control above it so context propagation into
-- the child is visible: tapping it flips the badge's own colors.
mountedComponentSection :: Theme -> Int -> View Theme Model Action
mountedComponentSection theme eventCount = section "vcomp \8212 statically-mounted child (props / context / model)"
  [ view_
    [ VE.onTap ToggleTheme
    , CSS.style_
      [ CSS.height "36px", CSS.marginBottom "6px"
      , CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "center"
      , CSS.backgroundColor (if theme == Dark then CSS.slategray else CSS.silver)
      ]
    ]
    [ text_ [ CSS.style_ [ CSS.color CSS.white, txtLine ] ]
        [ text (ms ("tap to toggle theme (currently " <> show theme <> ")")) ] ]
  , vcomp (BadgeProps eventCount) (static (mountStaticWithProps_ "gallery-badge" badgeComponent))
  ]
-----------------------------------------------------------------------------
-- | <input> + localStorage — exercises 'getLocalStorage' \/ 'setLocalStorage'
-- \/ 'clearLocalStorage' via the browser Web Storage API (see "Miso.Storage").
storageSection :: MisoString -> Maybe MisoString -> View context Model Action
storageSection inputVal loaded = section "localStorage \8212 save / load / clear"
  [ input_
    [ placeholder_ "value to save\8230"
    , textProp "value" inputVal
    , InE.onInput (\e -> StorageInputChanged (InE.inputValue e))
    , CSS.style_
      [ CSS.height "44px", CSS.width "100%", CSS.fontSize "16px"
      , CSS.backgroundColor CSS.white, CSS.padding "8px", CSS.marginBottom "6px"
      ]
    ]
  , view_
    [ CSS.style_ [ CSS.display "flex", CSS.flexDirection "row" ] ]
    [ storageButton CSS.seagreen "save" StorageSave
    , storageButton CSS.steelblue "load" StorageLoad
    , storageButton CSS.crimson "clear" StorageClear
    ]
  , text_
    [ CSS.style_ [ CSS.fontSize "13px", CSS.color CSS.black, CSS.marginTop "6px", txtLine ] ]
    [ text (ms ("loaded: " <> maybe "(nothing)" id loaded)) ]
  ]
  where
    storageButton bg lbl action = view_
      [ VE.onTap action
      , CSS.style_
        [ CSS.flexGrow 1, CSS.height "40px", CSS.marginRight "6px"
        , CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "center"
        , CSS.backgroundColor bg
        ]
      ]
      [ text_ [ CSS.style_ [ CSS.color CSS.white, txtLine ] ] [ text lbl ] ]
-----------------------------------------------------------------------------
