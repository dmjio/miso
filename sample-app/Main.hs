----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE CPP                #-}
----------------------------------------------------------------------------
-- | A tour of event decoders.
--
-- A 'Decoder' reads what it needs from the live DOM event, while the browser
-- is dispatching it. Besides plain properties (@event.clientX@), it can call
-- the event's methods (@event.getModifierState(\"CapsLock\")@), read data the
-- browser only exposes during dispatch (@event.clipboardData.getData(...)@),
-- and measure the element the handler belongs to
-- (@element.getBoundingClientRect()@).
--
-- Each section of the page pairs one decoder with an element to try it on,
-- and shows the last value it decoded. The decoders are defined, and
-- explained, in the "Decoders" part of this file.
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Control.Applicative ((<|>))
import           Data.List (intersperse)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
----------------------------------------------------------------------------
import           Miso
import           Miso.Html
import           Miso.Html.Property
import           Miso.Lens
import qualified Miso.String as S
----------------------------------------------------------------------------
-- | The last value each section decoded, by section key.
newtype Model = Model [(MisoString, MisoString)]
  deriving (Show, Eq)
----------------------------------------------------------------------------
data Action
  = Got MisoString MisoString
  -- ^ A section decoded something: the section's key, and what to show
  | ReadFile JSVal
  -- ^ Read a dropped file's contents (asynchronously, in 'updateModel')
----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
#ifdef INTERACTIVE
main = live pageEvents app
#else
main = startApp pageEvents app
#endif
----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
----------------------------------------------------------------------------
app :: App Model Action
app = component (Model []) updateModel viewModel
----------------------------------------------------------------------------
-- | The events this page listens to. miso delegates events from the mount
-- point, so every event a handler uses has to be in this map. @scroll@ does
-- not bubble, so it is listened to in the capture phase.
pageEvents :: Events
pageEvents = mconcat
  [ defaultEvents
  , keyboardEvents
  , mouseEvents
  , dragEvents
  , pointerEvents
  , clipboardEvents
  , touchEvents
  , M.fromList
      [ ("pointermove", BUBBLE)
      , ("wheel", BUBBLE)
      , ("scroll", CAPTURE)
      , ("focusin", BUBBLE)
      ]
  ]
----------------------------------------------------------------------------
updateModel :: Action -> Effect () () Model Action
updateModel = \case
  Got key value ->
    this %= \(Model results) -> Model ((key, value) : filter ((/= key) . fst) results)
  ReadFile file ->
    -- A File stays valid after the event, so it can be read here, later.
    -- Its contents only come back asynchronously, as a Promise.
    io $ do
      contents <- fromJSValUnchecked =<< await =<< (file # "text" $ ())
      pure (Got "drop-contents" (S.take 300 contents))
----------------------------------------------------------------------------
-- * Decoders
--
-- Every decoder starts at the event. 'member' moves to a property, 'invoke'
-- calls a method, 'atElement' moves to the element the handler is on,
-- 'atGlobal' to @window@, and 'fromJS' reads the value it has reached.
----------------------------------------------------------------------------
-- | @paste@: the pasted plain text,
-- @event.clipboardData.getData(\"text\/plain\")@.
--
-- This is 'clipboardTextDecoder', which 'onPaste' uses. The clipboard can
-- only be read while the paste event is being dispatched, which is when
-- decoders run.
pastedText :: Decoder MisoString
pastedText =
  member "clipboardData" (invoke "getData" ("text/plain" :: MisoString) fromJS)
----------------------------------------------------------------------------
-- | Shows a 'DataTransfer' decoded by 'clipboardDecoder' (as used by
-- 'onPasteData') or 'dropDecoder'. Copy some formatted text from another
-- page to see @text\/html@ next to @text\/plain@; paste a screenshot to see a
-- file.
describeDataTransfer :: DataTransfer -> MisoString
describeDataTransfer dt = commas $
  [ format <> " (" <> ms (S.length value) <> " chars)" | (format, value) <- dataTransferData dt ] <>
  [ "file " <> describeFile f | f <- dataTransferFiles dt ]
----------------------------------------------------------------------------
-- | @drop@: what was dropped, plus the dropped @File@ objects themselves, so
-- that 'updateModel' can read the first one's contents.
--
-- 'fromJS' at type 'JSVal' returns the value as it is. That is fine for a
-- @File@, which stays valid after the event; a @DataTransfer@ does not, so
-- it is decoded right away, by 'dropDecoder'.
dropped :: Decoder (DataTransfer, [JSVal])
dropped =
  (,) <$> dropDecoder
      <*> member "dataTransfer" (member "files" (items fromJS))
----------------------------------------------------------------------------
-- | @click@: where on the element the click landed, in percent of its width
-- and height.
--
-- @element.getBoundingClientRect()@ is measured as the click happens.
-- 'atElement' is the element with the handler. (@event.currentTarget@
-- would be the mount point, since miso delegates events, and
-- @event.target@ may be a child of the element.)
clickInElement :: Decoder (Double, Double)
clickInElement = do
  rect <- atElement (invoke "getBoundingClientRect" () domRectDecoder)
  x <- member "clientX" fromJS
  y <- member "clientY" fromJS
  pure ( 100 * (x - rectLeft rect) / rectWidth rect
       , 100 * (y - rectTop rect) / rectHeight rect
       )
----------------------------------------------------------------------------
-- | @keyup@: @event.getModifierState(\"CapsLock\")@
-- ('modifierStateDecoder'), for the usual \"Caps Lock is on\" warning on a
-- password field.
capsLock :: Decoder Bool
capsLock = invoke "getModifierState" ("CapsLock" :: MisoString) fromJS
----------------------------------------------------------------------------
-- | @keydown@: the key pressed, whether it is auto-repeating, and whether
-- Shift is held, the last one read with a method call.
keyDetails :: Decoder (MisoString, MisoString, Bool, Bool)
keyDetails =
  (,,,) <$> member "key" fromJS
        <*> member "code" fromJS
        <*> member "repeat" fromJS
        <*> invoke "getModifierState" ("Shift" :: MisoString) fromJS
----------------------------------------------------------------------------
-- | @mouseup@: the text selected in the page,
-- @window.getSelection().toString()@ ('selectionTextDecoder').
-- 'atGlobal' reads browser state at the moment of the event.
selectedText :: Decoder MisoString
selectedText = atGlobal (invoke "getSelection" () (invoke "toString" () fromJS))
----------------------------------------------------------------------------
-- | @select@ \/ @keyup@ \/ @click@ in a text field: the caret, or the
-- selected range, @(target.selectionStart, target.selectionEnd)@.
--
-- This is 'inputSelectionDecoder'. A JSON snapshot of the event ('at') would
-- not have these: miso leaves them out of it, because Safari throws when
-- they are read on some inputs.
caret :: Decoder (Int, Int)
caret =
  member "target" $
    (,) <$> member "selectionStart" fromJS
        <*> member "selectionEnd" fromJS
----------------------------------------------------------------------------
-- | @change@ on a @\<select multiple\>@: the value of every selected
-- option, from @target.selectedOptions@, an @HTMLCollection@
-- ('selectedOptionsDecoder').
selectedOptions :: Decoder [MisoString]
selectedOptions =
  members ["target", "selectedOptions"] (items (member "value" fromJS))
----------------------------------------------------------------------------
-- | @input@: whether the field is valid and, if not, the browser's message.
-- @target.validity@ is an object, which 'members' walks into.
validity :: Decoder (Bool, MisoString)
validity =
  member "target" $
    (,) <$> members ["validity", "valid"] fromJS
        <*> member "validationMessage" fromJS
----------------------------------------------------------------------------
-- | @input@: what kind of edit happened (@insertText@,
-- @deleteContentBackward@, @insertFromPaste@, ...) and the inserted text,
-- which is @null@ for deletions, hence 'nullable'.
inputDetails :: Decoder (MisoString, Maybe MisoString)
inputDetails =
  (,) <$> member "inputType" fromJS
      <*> member "data" (nullable fromJS)
----------------------------------------------------------------------------
-- | @click@: the nodes the event travelled through, innermost first,
-- @event.composedPath()@. The last one is @window@, which has no
-- @nodeName@, so '<|>' falls back to \"window\".
eventPath :: Decoder [MisoString]
eventPath =
  invoke "composedPath" () (items (member "nodeName" fromJS <|> pure "window"))
----------------------------------------------------------------------------
-- | @click@ anywhere in a list: the @id@ of the @\<li\>@ the click was in,
-- @event.target.closest(\"li\").id@, so one handler on the list serves
-- every item. @closest@ returns @null@ outside an item, hence 'nullable'.
clickedItem :: Decoder (Maybe MisoString)
clickedItem =
  member "target" $
    invoke "closest" ("li" :: MisoString) (nullable (member "id" fromJS))
----------------------------------------------------------------------------
-- | @click@: the element's background colour as the browser computed it,
-- @window.getComputedStyle(element).backgroundColor@.
--
-- The element is read first, then passed as an argument: decoders compose
-- with @do@.
computedBackground :: Decoder MisoString
computedBackground = do
  element <- atElement fromJS
  atGlobal (invoke "getComputedStyle" (element :: JSVal) (member "backgroundColor" fromJS))
----------------------------------------------------------------------------
-- | @pointermove@: how many pointer events the browser merged into this
-- one, @event.getCoalescedEvents().length@. In a browser without
-- @getCoalescedEvents@, 'invoke' fails and '<|>' falls back to 1.
coalescedCount :: Decoder Int
coalescedCount =
  invoke "getCoalescedEvents" () (member "length" fromJS) <|> pure 1
----------------------------------------------------------------------------
-- | @scroll@: how far the element is scrolled, and whether it reached the
-- bottom, from its @scrollTop@, @scrollHeight@ and @clientHeight@.
scrollPosition :: Decoder (Double, Bool)
scrollPosition = atElement $ do
  top <- member "scrollTop" fromJS
  height <- member "scrollHeight" fromJS
  visible <- member "clientHeight" fromJS
  pure (top, top + visible >= height - 1)
----------------------------------------------------------------------------
-- | @wheel@: @deltaY@ and its unit, @deltaMode@ (0: pixels, 1: lines,
-- 2: pages).
wheelDelta :: Decoder (Double, Int)
wheelDelta = (,) <$> member "deltaY" fromJS <*> member "deltaMode" fromJS
----------------------------------------------------------------------------
-- | @touchstart@ \/ @touchmove@: every finger on the screen,
-- @event.touches@, a @TouchList@ ('items' reads any array-like).
touchPoints :: Decoder [(Double, Double)]
touchPoints =
  member "touches" $
    items ((,) <$> member "clientX" fromJS <*> member "clientY" fromJS)
----------------------------------------------------------------------------
-- | @focusin@: the @id@ of the element that now has focus,
-- @document.activeElement.id@.
focusedId :: Decoder MisoString
focusedId = atGlobal (members ["document", "activeElement", "id"] fromJS)
----------------------------------------------------------------------------
-- | Any event: properties every event has.
eventBasics :: Decoder (MisoString, Double, Bool)
eventBasics =
  (,,) <$> member "type" fromJS
       <*> member "timeStamp" fromJS
       <*> member "isTrusted" fromJS
----------------------------------------------------------------------------
-- * View
----------------------------------------------------------------------------
viewModel :: Model -> View () () Model Action
viewModel m =
  div_ [ class_ "page" ]
  [ h1_ [] [ "Decoding events" ]
  , p_ []
    [ "Each section reads something from a DOM event while the browser "
    , "dispatches it. The decoders are in sample-app/Main.hs."
    ]
  , demo m "paste" "Pasted text"
      "event.clipboardData.getData(\"text/plain\")"
      [ textarea_ [ placeholder_ "Paste here", on "paste" pastedText (\t _ _ -> Got "paste" t) ] ]
  , demo m "paste-data" "Everything pasted"
      "event.clipboardData: types, getData(type) for each, files"
      [ textarea_ [ placeholder_ "Paste formatted text, or an image"
                  , onPasteData (Got "paste-data" . describeDataTransfer)
                  ]
      ]
  , demo m "drop" "Dropped files and data"
      "event.dataTransfer (dragover only shows the types, drop shows everything)"
      [ div_
        [ class_ "dropzone"
          -- An element only accepts a drop if dragover's default is prevented.
        , onWithOptions BUBBLE preventDefault "dragover" dropDecoder $ \dt _ _ ->
            Got "drop" ("dragging: " <> commas (dataTransferTypes dt))
        , onWithOptions BUBBLE preventDefault "drop" dropped (\d _ _ -> droppedAction d)
        ]
        [ "Drop a text file, or some selected text, here" ]
      , pre_ [ class_ "out" ] [ text (resultFor "drop-contents" m) ]
      ]
  , demo m "files" "Chosen files"
      "event.target.files"
      [ input_ [ type_ "file", multiple_ True, onFiles (Got "files" . commas . map describeFile) ] ]
  , demo m "click" "Where the click landed"
      "element.getBoundingClientRect(), event.clientX, event.clientY"
      [ div_ [ class_ "box", on "click" clickInElement $ \(x, y) _ _ ->
                 Got "click" (percent x <> " across, " <> percent y <> " down")
             ]
        [ span_ [ class_ "inner" ] [ "click anywhere, even here" ] ]
      ]
  , demo m "caps" "Caps Lock"
      "event.getModifierState(\"CapsLock\")"
      [ input_ [ type_ "password", placeholder_ "Type a password"
               , on "keyup" capsLock $ \isOn _ _ ->
                   Got "caps" (if isOn then "Caps Lock is on" else "Caps Lock is off")
               ]
      ]
  , demo m "key" "Key details"
      "event.key, event.code, event.repeat, event.getModifierState(\"Shift\")"
      [ input_ [ placeholder_ "Press keys", on "keydown" keyDetails $ \(k, c, r, shift) _ _ ->
                   Got "key" (commas [ "key " <> k, "code " <> c, "repeat " <> yesNo r, "shift " <> yesNo shift ])
               ]
      ]
  , demo m "selection" "Selected text"
      "window.getSelection().toString()"
      [ p_ [ on "mouseup" selectedText $ \s _ _ -> Got "selection" s ]
        [ "Select some words in this sentence with the mouse, and they show up below." ]
      ]
  , demo m "caret" "Caret in a text field"
      "event.target.selectionStart, event.target.selectionEnd"
      [ textarea_ [ placeholder_ "Type, click or select"
                  , on "keyup" caret (\s _ _ -> Got "caret" (describeCaret s))
                  , on "click" caret (\s _ _ -> Got "caret" (describeCaret s))
                  , on "select" caret (\s _ _ -> Got "caret" (describeCaret s))
                  ]
      ]
  , demo m "options" "Selected options"
      "event.target.selectedOptions"
      [ select_ [ multiple_ True, on "change" selectedOptions (\vs _ _ -> Got "options" (commas vs)) ]
        [ option_ [ value_ v ] [ text v ] | v <- [ "red", "green", "blue", "yellow" ] ]
      ]
  , demo m "validity" "Form validation"
      "event.target.validity.valid, event.target.validationMessage"
      [ input_ [ type_ "email", required_ True, placeholder_ "An email address"
               , on "input" validity $ \(valid, message) _ _ ->
                   Got "validity" (if valid then "valid" else message)
               ]
      ]
  , demo m "input-type" "What kind of edit"
      "event.inputType, event.data"
      [ input_ [ placeholder_ "Type, delete, paste", on "input" inputDetails $ \(kind, inserted) _ _ ->
                   Got "input-type" (kind <> maybe "" (": " <>) inserted)
               ]
      ]
  , demo m "path" "Event path"
      "event.composedPath()"
      [ div_ [ class_ "box", on "click" eventPath (\p _ _ -> Got "path" (S.intercalate " > " p)) ]
        [ span_ [ class_ "inner" ] [ b_ [] [ "click the bold text" ], " or around it" ] ]
      ]
  , demo m "item" "Which list item"
      "event.target.closest(\"li\").id"
      [ ul_ [ on "click" clickedItem (\i _ _ -> Got "item" (maybe "not in an item" ("clicked " <>) i)) ]
        [ li_ [ id_ ("item-" <> ms n) ] [ "Item ", b_ [] [ text (ms n) ] ] | n <- [ 1 .. 3 :: Int ] ]
      ]
  , demo m "style" "Computed style"
      "window.getComputedStyle(element).backgroundColor"
      [ div_ [ class_ "box tinted", on "click" computedBackground (\c _ _ -> Got "style" c) ]
        [ "click to read my background colour" ]
      ]
  , demo m "coalesced" "Coalesced pointer events"
      "event.getCoalescedEvents().length"
      [ div_ [ class_ "box", on "pointermove" coalescedCount (\n _ _ -> Got "coalesced" (ms n <> " event(s) in this one")) ]
        [ "move the pointer quickly over here" ]
      ]
  , demo m "scroll" "Scroll position"
      "element.scrollTop, element.scrollHeight, element.clientHeight"
      [ div_ [ class_ "scroller", on "scroll" scrollPosition $ \(top, bottom) _ _ ->
                 Got "scroll" (ms (round top :: Int) <> "px" <> (if bottom then ", at the bottom" else ""))
             ]
        [ p_ [] [ text ("Line " <> ms n) ] | n <- [ 1 .. 30 :: Int ] ]
      ]
  , demo m "wheel" "Mouse wheel"
      "event.deltaY, event.deltaMode"
      [ div_ [ class_ "box", on "wheel" wheelDelta $ \(dy, mode) _ _ ->
                 Got "wheel" (ms dy <> wheelUnit mode)
             ]
        [ "turn the mouse wheel over here" ]
      ]
  , demo m "touch" "Touches"
      "event.touches"
      [ div_ [ class_ "box"
             , on "touchstart" touchPoints (\ts _ _ -> Got "touch" (describeTouches ts))
             , on "touchmove" touchPoints (\ts _ _ -> Got "touch" (describeTouches ts))
             ]
        [ "touch here with one or more fingers (on a touch screen)" ]
      ]
  , demo m "focus" "Focused element"
      "document.activeElement.id"
      [ div_ [ on "focusin" focusedId (\i _ _ -> Got "focus" ("focused #" <> i)) ]
        [ input_ [ id_ "first-name", placeholder_ "First name" ]
        , input_ [ id_ "last-name", placeholder_ "Last name" ]
        ]
      ]
  , demo m "basics" "Event basics"
      "event.type, event.timeStamp, event.isTrusted"
      [ button_ [ on "click" eventBasics $ \(t, stamp, trusted) _ _ ->
                    Got "basics" (commas [ t, "at " <> ms (round stamp :: Int) <> "ms", if trusted then "from the user" else "synthetic" ])
                ]
        [ "Click me" ]
      ]
  ]
----------------------------------------------------------------------------
-- | One section: a title, what the decoder reads, something to try it on,
-- and the last value it decoded.
demo
  :: Model
  -> MisoString
  -- ^ Key of the result to show
  -> MisoString
  -- ^ Title
  -> MisoString
  -- ^ What the decoder reads, in JavaScript terms
  -> [View () () Model Action]
  -- ^ Something to try it on
  -> View () () Model Action
demo m key title reads' body =
  section_ [ class_ "demo" ]
  [ h2_ [] [ text title ]
  , p_ [ class_ "reads" ] [ code_ [] [ text reads' ] ]
  , div_ [ class_ "try" ] body
  , pre_ [ class_ "out" ] [ text (resultFor key m) ]
  ]
----------------------------------------------------------------------------
-- | Read the first dropped file, or else show what was dropped.
droppedAction :: (DataTransfer, [JSVal]) -> Action
droppedAction (_, file : _) = ReadFile file
droppedAction (dt, []) = Got "drop" (describeDataTransfer dt)
----------------------------------------------------------------------------
resultFor :: MisoString -> Model -> MisoString
resultFor key (Model results) = fromMaybe "" (lookup key results)
----------------------------------------------------------------------------
commas :: [MisoString] -> MisoString
commas = mconcat . intersperse ", "
----------------------------------------------------------------------------
yesNo :: Bool -> MisoString
yesNo b = if b then "yes" else "no"
----------------------------------------------------------------------------
percent :: Double -> MisoString
percent x = ms (round x :: Int) <> "%"
----------------------------------------------------------------------------
wheelUnit :: Int -> MisoString
wheelUnit = \case
  0 -> " pixels"
  1 -> " lines"
  _ -> " pages"
----------------------------------------------------------------------------
describeFile :: FileInfo -> MisoString
describeFile f =
  fileName f <> " (" <> fileType f <> ", " <> ms (round (fileSize f) :: Int) <> " bytes)"
----------------------------------------------------------------------------
describeCaret :: (Int, Int) -> MisoString
describeCaret (start, end)
  | start == end = "caret at " <> ms start
  | otherwise = "selected " <> ms start <> " to " <> ms end
----------------------------------------------------------------------------
describeTouches :: [(Double, Double)] -> MisoString
describeTouches ts =
  ms (length ts) <> " finger(s): " <>
    commas [ "(" <> ms (round x :: Int) <> ", " <> ms (round y :: Int) <> ")" | (x, y) <- ts ]
----------------------------------------------------------------------------
