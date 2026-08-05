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
  deriving stock (Generic)
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
  } deriving stock (Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
-----------------------------------------------------------------------------
main :: IO ()
main = native (nativeEvents <> nativeXEvents) (static (mountStatic_ galleryComponent))
-----------------------------------------------------------------------------
galleryComponent :: App Model Action
galleryComponent = component (Model [] False False False 6) updateModel viewModel
-----------------------------------------------------------------------------
updateModel :: Action -> Effect context props Model Action
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
-----------------------------------------------------------------------------
viewModel :: context -> props -> Model -> View context Model Action
viewModel _ _ Model{..} = view_
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
