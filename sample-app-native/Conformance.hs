-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StaticPointers     #-}
-----------------------------------------------------------------------------
-- | Deterministic dual-thread conformance fixture using only Lynx core
-- @<view>@ and @<text>@ elements. Optional gallery behaviors must not be able
-- to truncate the tree before IFR, BTS, MTS, churn, or lifecycle probes run.
module Main where
-----------------------------------------------------------------------------
import           GHC.Generics (Generic)
import           Miso hiding (text_)
import qualified Miso.CSS as CSS
import           Miso.Html.Property (id_)
import           Miso.JSON (FromJSON, ToJSON)
import           Miso.Native
import qualified Miso.Native.Element.View.Event as VE
import           Miso.Native.MainThread (setStyleProperty)
-----------------------------------------------------------------------------
data Action
  = IncrementBTS
  | ToggleDynamic
  | PaintMTS DOMRef
  | ReadMTS DOMRef Int
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data Model = Model
  { probeCount     :: Int
  , dynamicVisible :: Bool
  , lastEvent      :: MisoString
  } deriving stock (Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
-----------------------------------------------------------------------------
main :: IO ()
main =
  nativeWithContext nativeEvents ()
    (static (mountStatic_ probeComponent))

probeComponent :: Component () () Model Action
probeComponent = component initialModel updateModel viewModel
  where
    initialModel = Model
      { probeCount = 0
      , dynamicVisible = True
      , lastEvent = "boot"
      }

updateModel :: Action -> Effect () () Model Action
updateModel = \case
  IncrementBTS ->
    modify $ \m -> m
      { probeCount = probeCount m + 1
      , lastEvent = "bts: increment"
      }
  ToggleDynamic ->
    modify $ \m -> m
      { dynamicVisible = not (dynamicVisible m)
      , lastEvent = "bts: toggle dynamic"
      }
  PaintMTS domRef ->
    io_ (setStyleProperty domRef "background-color" "#f59e0b")
  ReadMTS domRef n ->
    io_ (setStyleProperty domRef "opacity" (opacityForCount n))

viewModel :: () -> () -> Model -> View () Model Action
viewModel _ _ Model{..} =
  view_
    [ id_ "probe-root"
    , CSS.style_
      [ CSS.width "100%"
      , CSS.height "100vh"
      , CSS.display "flex"
      , CSS.flexDirection "column"
      , CSS.padding "24px"
      , CSS.backgroundColor (CSS.RGB 15 23 42)
      ]
    ]
    [ text_
      [ id_ "probe-title"
      , CSS.style_
        [ CSS.fontSize "24px"
        , CSS.fontWeight "700"
        , CSS.color CSS.white
        , lineHeight
        , CSS.marginBottom "12px"
        ]
      ]
      [ text "Miso Lynx dual-thread conformance" ]
    , text_
      [ id_ "probe-status"
      , CSS.style_
        [ CSS.fontSize "15px"
        , CSS.color CSS.lime
        , lineHeight
        , CSS.marginBottom "12px"
        ]
      ]
      [ text (ms
          ( "count=" <> show probeCount
         <> " dynamic=" <> show dynamicVisible
         <> " last=" <> fromMisoString lastEvent
          ))
      ]
    , btsIncrementButton
    , dynamicSection dynamicVisible
    , mtsPaintSection
    , mtsReadSection
    , childSection probeCount
    ]

lineHeight :: CSS.Style
lineHeight = CSS.lineHeight "1.4"

panelStyle :: [CSS.Style]
panelStyle =
  [ CSS.width "100%"
  , CSS.height "64px"
  , CSS.flexShrink 0
  , CSS.display "flex"
  , CSS.alignItems "center"
  , CSS.justifyContent "center"
  , CSS.marginBottom "10px"
  , CSS.borderRadius "10px"
  ]

btsIncrementButton :: View context Model Action
btsIncrementButton =
  view_
    [ id_ "bts-increment"
    , VE.onTap IncrementBTS
    , CSS.style_ (panelStyle <> [CSS.backgroundColor CSS.steelblue])
    ]
    [ label "tap: BTS increment + declarative rerender" ]

dynamicSection :: Bool -> View context Model Action
dynamicSection visible =
  view_
    [ id_ "dynamic-section"
    , CSS.style_
      [ CSS.width "100%"
      , CSS.flexShrink 0
      , CSS.display "flex"
      , CSS.flexDirection "column"
      ]
    ]
    [ view_
      [ id_ "dynamic-toggle"
      , VE.onTap ToggleDynamic
      , CSS.style_ (panelStyle <> [CSS.backgroundColor CSS.seagreen])
      ]
      [ label "tap: BTS remove/insert dynamic child" ]
    , view_
      [ id_ "dynamic-slot"
      , CSS.style_
        [ CSS.width "100%"
        , CSS.height "52px"
        , CSS.flexShrink 0
        , CSS.marginBottom "10px"
        ]
      ]
      (if visible then [dynamicChild] else [])
    ]

dynamicChild :: View context Model Action
dynamicChild =
  view_
    [ id_ "dynamic-child"
    , CSS.style_
      [ CSS.width "100%"
      , CSS.height "52px"
      , CSS.display "flex"
      , CSS.alignItems "center"
      , CSS.justifyContent "center"
      , CSS.backgroundColor CSS.darkslategray
      , CSS.borderRadius "10px"
      ]
    ]
    [ label "dynamic child is mounted" ]

mtsPaintSection :: View context Model Action
mtsPaintSection =
  view_
    [ id_ "mts-paint-shell"
    , CSS.style_ (panelStyle <> [CSS.backgroundColor (CSS.RGB 51 65 85)])
    ]
    [ view_
      [ id_ "mts-paint"
      , event (static (VE.onTapMainWith PaintMTS))
      , CSS.style_
        [ CSS.width "94%"
        , CSS.height "48px"
        , CSS.display "flex"
        , CSS.alignItems "center"
        , CSS.justifyContent "center"
        , CSS.borderRadius "8px"
        ]
      ]
      [ label "tap: MTS-only background-color" ]
    ]

mtsReadSection :: View context Model Action
mtsReadSection =
  view_
    [ id_ "mts-read-shell"
    , CSS.style_ (panelStyle <> [CSS.backgroundColor CSS.mediumpurple])
    ]
    [ view_
      [ id_ "mts-read"
      , event (static
          (onMain "tap" emptyDecoder
            (\() m domRef -> ReadMTS domRef (probeCount m))))
      , CSS.style_
        [ CSS.width "94%"
        , CSS.height "48px"
        , CSS.display "flex"
        , CSS.alignItems "center"
        , CSS.justifyContent "center"
        ]
      ]
      [ label "tap: MTS reads hydrated BTS count" ]
    ]

label :: MisoString -> View context model action
label value =
  text_
    [ CSS.style_
      [ CSS.fontSize "15px"
      , CSS.fontWeight "600"
      , CSS.color CSS.white
      , lineHeight
      ]
    ]
    [ text value ]
-----------------------------------------------------------------------------
data ChildProps = ChildProps
  { parentCount :: Int
  } deriving stock (Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ChildModel = ChildModel
  { childCount :: Int
  } deriving stock (Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ChildAction = IncrementChild
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

childComponent :: Component () ChildProps ChildModel ChildAction
childComponent = component (ChildModel 0) childUpdate childView

childUpdate :: ChildAction -> Effect () ChildProps ChildModel ChildAction
childUpdate IncrementChild =
  modify $ \m -> m { childCount = childCount m + 1 }

childView :: () -> ChildProps -> ChildModel -> View () ChildModel ChildAction
childView _ ChildProps{..} ChildModel{..} =
  view_
    [ id_ "child-button"
    , VE.onTap IncrementChild
    , CSS.style_ (panelStyle <> [CSS.backgroundColor CSS.indigo])
    ]
    [ text_
      [ id_ "child-status"
      , CSS.style_
        [ CSS.fontSize "15px"
        , CSS.fontWeight "600"
        , CSS.color CSS.white
        , lineHeight
        ]
      ]
      [ text (ms
          ( "tap child: parentCount=" <> show parentCount
         <> " childCount=" <> show childCount
          ))
      ]
    ]

childSection :: Int -> View () Model Action
childSection count =
  view_
    [ id_ "child-slot"
    , CSS.style_
      [ CSS.width "100%"
      , CSS.flexShrink 0
      ]
    ]
    [ vcomp (ChildProps count) (static (mountStaticWithProps childComponent)) ]

opacityForCount :: Int -> MisoString
opacityForCount n =
  ms (show (max 0.35 (min 1 (0.35 + fromIntegral n * 0.13)) :: Double))
-----------------------------------------------------------------------------
