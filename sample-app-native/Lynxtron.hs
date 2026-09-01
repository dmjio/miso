-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StaticPointers     #-}
-----------------------------------------------------------------------------
-- | Lynxtron desktop sample. Exercises every channel of the card ↔ Node bridge
-- from "Miso.Native.Lynxtron":
--
--   * 'invokeNode'   — ask the main process for its @os@ info (request/reply)
--   * 'sendNode'     — log a line to the main process' stdout (fire-and-forget)
--   * 'callExposed'  — call @contextBridge.exposeInLynxBTS@ functions from
--                      @desktop/preload.js@ (sync + async, nested)
--   * 'nodeEventSub' — receive the 1 Hz @tick@ the main process pushes with
--                      @win.sendGlobalEvent@
--
-- The Node side lives in @desktop/main.js@ and @desktop/preload.js@. The same
-- bundle still boots on iOS / Android; 'isLynxtron' reports which host we're on.
module Main where
-----------------------------------------------------------------------------
import           GHC.Generics (Generic)
import           Miso hiding (text_)
import qualified Miso.CSS as CSS
import           Miso.Html.Property (id_)
import           Miso.JSON (FromJSON, ToJSON, Value(..), object, (.=))
import           Miso.Native
import           Miso.Native.Lynxtron
import qualified Miso.Native.Element.View.Event as VE
-----------------------------------------------------------------------------
data Action
  = Boot
  | Detected Bool
  | AskOs
  | GotOs OsInfo
  | Greet
  | Greeted MisoString
  | CheckFile
  | FileChecked Bool
  | LogLine
  | Tick Int
  | BridgeError MisoString
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Reply shape of the @os-info@ handler in @desktop/main.js@.
data OsInfo = OsInfo
  { platform :: MisoString
  , arch     :: MisoString
  , hostname :: MisoString
  , node     :: MisoString
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Model = Model
  { desktop   :: Maybe Bool
  , osInfo    :: Maybe OsInfo
  , greeting  :: MisoString
  , fileState :: MisoString
  , ticks     :: Int
  , sent      :: Int
  , lastError :: MisoString
  } deriving stock (Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
-----------------------------------------------------------------------------
main :: IO ()
main =
  nativeWithContext nativeEvents ()
    (static (mountStatic app))

app :: Component () () Model Action
app = (component initialModel updateModel viewModel)
  { subs        = [ nodeEventSub "tick" (either BridgeError Tick) ]
  , mount       = Just Boot
  }
  where
    initialModel = Model
      { desktop   = Nothing
      , osInfo    = Nothing
      , greeting  = "-"
      , fileState = "-"
      , ticks     = 0
      , sent      = 0
      , lastError = ""
      }
-----------------------------------------------------------------------------
updateModel :: Action -> Effect () () Model Action
updateModel = \case
  Boot ->
    io (Detected <$> isLynxtron)
  Detected b ->
    modify $ \m -> m { desktop = Just b }

  -- card -> node, request/reply (lynxBridge.handle('os-info', ...))
  AskOs ->
    io_' $ \sink -> invokeNode "os-info" (object []) (sink . either BridgeError GotOs)
  GotOs info ->
    modify $ \m -> m { osInfo = Just info, lastError = "" }

  -- node -> card API (contextBridge.exposeInLynxBTS, async fn)
  Greet ->
    io_' $ \sink -> callExposedWith "greet" [String "miso"] (sink . either BridgeError Greeted)
  Greeted s ->
    modify $ \m -> m { greeting = s, lastError = "" }

  -- node -> card API, nested object + Promise
  CheckFile ->
    io_' $ \sink -> callExposedWith "fileApi.exists" [String "package.json"] (sink . either BridgeError FileChecked)
  FileChecked b ->
    modify $ \m -> m { fileState = if b then "package.json exists" else "package.json missing", lastError = "" }

  -- card -> node, fire-and-forget (win.on('-lynx-message'))
  LogLine -> do
    n <- (+ 1) . sent <$> get
    modify $ \m -> m { sent = n }
    io_ $ sendNode "log" (object [ "line" .= ("hello from miso #" <> ms (show n) :: MisoString) ])

  -- node -> card global event (win.sendGlobalEvent('tick', n))
  Tick n ->
    modify $ \m -> m { ticks = n }

  BridgeError e ->
    modify $ \m -> m { lastError = e }
  where
    -- run a BTS IO action that dispatches its result back through the sink
    io_' = withSink
-----------------------------------------------------------------------------
viewModel :: () -> () -> Model -> View () Model Action
viewModel _ _ Model{..} =
  view_
    [ id_ "root"
    , CSS.style_
      [ CSS.width "100%"
      , CSS.height "100vh"
      , CSS.display "flex"
      , CSS.flexDirection "column"
      , CSS.padding "24px"
      , CSS.backgroundColor (CSS.RGB 15 23 42)
      ]
    ]
    [ heading "miso × Lynxtron"
    , statusLine $ case desktop of
        Nothing    -> "detecting host…"
        Just True  -> "host: Lynxtron (NativeModules.bridge present)"
        Just False -> "host: not Lynxtron — bridge calls will no-op"
    , statusLine ("ticks from main process: " <> ms (show ticks))
    , tapButton "ask-os" "invokeNode \"os-info\"" AskOs CSS.steelblue
    , statusLine $ case osInfo of
        Nothing        -> "os: -"
        Just OsInfo{..} -> "os: " <> platform <> "/" <> arch <> " on " <> hostname <> " (node " <> node <> ")"
    , tapButton "greet" "callExposed \"greet\" (async)" Greet CSS.seagreen
    , statusLine ("greeting: " <> greeting)
    , tapButton "check-file" "callExposed \"fileApi.exists\" (nested)" CheckFile CSS.darkslategray
    , statusLine ("file: " <> fileState)
    , tapButton "log" "sendNode \"log\" (fire-and-forget)" LogLine (CSS.RGB 124 58 237)
    , statusLine ("sent: " <> ms (show sent) <> " — check the terminal running lynxtron")
    , errorLine lastError
    ]
-----------------------------------------------------------------------------
heading :: MisoString -> View context Model Action
heading s =
  text_
    [ CSS.style_
      [ CSS.fontSize "24px", CSS.fontWeight "700", CSS.color CSS.white
      , CSS.lineHeight "1.4", CSS.marginBottom "12px" ]
    ]
    [ text s ]

statusLine :: MisoString -> View context Model Action
statusLine s =
  text_
    [ CSS.style_
      [ CSS.fontSize "14px", CSS.color CSS.lime, CSS.lineHeight "1.4"
      , CSS.marginBottom "10px" ]
    ]
    [ text s ]

errorLine :: MisoString -> View context Model Action
errorLine "" = view_ [] []
errorLine e =
  text_
    [ CSS.style_ [ CSS.fontSize "13px", CSS.color CSS.tomato, CSS.lineHeight "1.4" ] ]
    [ text ("error: " <> e) ]

tapButton :: MisoString -> MisoString -> Action -> CSS.Color -> View context Model Action
tapButton ident caption action bg =
  view_
    [ id_ ident
    , VE.onTap action
    , CSS.style_
      [ CSS.width "100%", CSS.height "56px", CSS.flexShrink 0
      , CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "center"
      , CSS.marginBottom "8px", CSS.borderRadius "10px", CSS.backgroundColor bg
      ]
    ]
    [ text_
      [ CSS.style_ [ CSS.fontSize "15px", CSS.fontWeight "600", CSS.color CSS.white ] ]
      [ text caption ]
    ]
-----------------------------------------------------------------------------
