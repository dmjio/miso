-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Event.Decoder
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Event.Decoder" provides t'Decoder', which tells miso how to extract a
-- Haskell value from a browser
-- <https://developer.mozilla.org/en-US/docs/Web/API/Event DOM Event>.
--
-- Decoders are consumed by 'Miso.Event.on' (and 'Miso.Subscription.Window.windowSub'):
--
-- @
-- on :: 'Miso.String.MisoString'                        -- event name (e.g. \"paste\")
--    -> t'Decoder' result                         -- what to read from the event
--    -> (result -> model -> DOMRef -> action)   -- turn it into an action
--    -> 'Miso.Types.Attribute' model action
-- @
--
-- = Decoders read the live event, during dispatch
--
-- A t'Decoder' is a read-only observation of the event, made __while the
-- browser is dispatching it__. That is when event data is valid: a
-- <https://developer.mozilla.org/en-US/docs/Web/API/DataTransfer DataTransfer>
-- (@event.clipboardData@, @event.dataTransfer@) can only be read during
-- dispatch, and an element's size or the current text selection are only
-- meaningful at that moment. Your @update@ function runs later, so it is too
-- late to read them there.
--
-- Building a t'Decoder' does nothing: it is a value, like an aeson
-- @Parser@. miso runs it once per event, synchronously, inside the event
-- listener. There is no way to run arbitrary 'IO' in a t'Decoder' (no
-- @MonadIO@, no exported constructor), so a 'Miso.Types.View' that
-- builds decoders stays pure.
--
-- = A cursor into the event
--
-- A t'Decoder' always looks at one JavaScript value, the /focus/. It starts
-- at the event. These combinators move the focus:
--
-- ['member' k]     @focus[k]@ (a property, own or inherited, including getters)
-- ['members' ks]   a path of properties: @'members' [\"a\", \"b\"] = 'member' \"a\" . 'member' \"b\"@
-- ['item' i]       @focus[i]@ of an array or array-like
-- ['invoke' m args] the result of the method call @focus.m(...args)@
-- ['atEvent']      the event itself
-- ['atElement']    the element whose handler is running (see below)
-- ['atGlobal']     @globalThis@ (@window@ in a browser)
--
-- and these read it:
--
-- ['fromJS']      convert the focus with 'Miso.DSL.FromJSVal' ('MisoString', 'Int', 'Double', 'Bool', 'JSVal', ...)
-- ['parseWith']   snapshot the focus as JSON and run a 'Miso.JSON.Parser' on it
-- ['items']       decode every element of an array-like (@Array@, @FileList@, @TouchList@, @NodeList@, ...)
-- ['nullable']    'Nothing' when the focus is @null@ or @undefined@
--
-- t'Decoder' is a 'Monad' and an 'Alternative': combine reads with
-- @\<$\>@ \/ @\<*\>@ \/ @do@, and give fallbacks with @\<|\>@. Any failure,
-- including a JavaScript exception (e.g. a method that doesn't exist in this
-- browser), makes the decoder fail, and @\<|\>@ moves on to the next choice.
--
-- @
-- -- event.clipboardData.getData(\"text\/plain\")
-- pastedText :: t'Decoder' 'MisoString'
-- pastedText = 'member' \"clipboardData\" ('invoke' \"getData\" (\"text\/plain\" :: 'MisoString') 'fromJS')
--
-- -- Where the click landed, relative to the element that was clicked
-- clickInElement :: t'Decoder' (Double, Double)
-- clickInElement = do
--   rect <- 'atElement' ('invoke' \"getBoundingClientRect\" () 'domRectDecoder')
--   x    <- 'member' \"clientX\" 'fromJS'
--   y    <- 'member' \"clientY\" 'fromJS'
--   pure (x - 'rectLeft' rect, y - 'rectTop' rect)
--
-- -- Coalesced pointer events where the browser has them, else just this one
-- coalesced :: t'Decoder' [(Double, Double)]
-- coalesced = 'invoke' \"getCoalescedEvents\" () ('items' xy) \<|\> fmap pure xy
--   where xy = (,) \<$\> 'member' \"clientX\" 'fromJS' \<*\> 'member' \"clientY\" 'fromJS'
-- @
--
-- == The element
--
-- miso delegates events: the browser's listener is on the mount point, so
-- @event.currentTarget@ is the mount point, not your element. 'atElement'
-- focuses the element that the handler was attached to in your
-- 'Miso.Types.View' (the same element as the 'Miso.Types.DOMRef' passed to
-- the handler). @event.target@ is still the innermost element the event
-- happened on, which may be a child of yours.
--
-- == Methods should only read
--
-- 'invoke' calls whatever method you name. Use it for methods that read
-- (@getData@, @getBoundingClientRect@, @getModifierState@, @closest@,
-- @composedPath@, @matches@, ...). A method with an effect, such as
-- @preventDefault@ or @remove@, runs every time the event fires and is not
-- what a decoder is for: use 'Miso.Event.Types.Options' for
-- @preventDefault@ \/ @stopPropagation@, and effects in @update@ for the rest.
--
-- == Reading the focus with 'fromJS'
--
-- 'fromJS' uses the 'Miso.DSL.FromJSVal' instance of the result type. It
-- fails on @null@ and @undefined@ (wrap it in 'nullable' to accept them).
-- Some backends convert without checking the JavaScript type, so reading a
-- number as a 'MisoString' is not guaranteed to fail: read each value at its
-- real type.
--
-- 'fromJS' at type 'JSVal' hands back the focus itself, e.g. a @File@ to read
-- later in @update@. Only keep a 'JSVal' that stays valid after dispatch
-- (a @File@ or an element do; a @DataTransfer@ does not).
--
-- = Built-in decoders
--
-- ['emptyDecoder'] @()@ — @click@, @submit@, stateless events
-- ['keycodeDecoder'] 'Miso.Event.Types.KeyCode' — @keydown@ \/ @keyup@ key code
-- ['keyInfoDecoder'] 'Miso.Event.Types.KeyInfo' — key code + modifier keys
-- ['valueDecoder'] 'Miso.String.MisoString' — @input@ \/ @change@ (@event.target.value@)
-- ['checkedDecoder'] 'Miso.Event.Types.Checked' — checkbox @change@ (@event.target.checked@)
-- ['pointerDecoder'] 'Miso.Event.Types.PointerEvent' — pointer\/mouse position and metadata
-- ['clipboardTextDecoder'] 'Miso.String.MisoString' — @paste@ (@event.clipboardData.getData(\"text\/plain\")@)
-- ['clipboardDecoder'] 'Miso.Event.Types.DataTransfer' — @paste@ \/ @copy@ \/ @cut@, every format
-- ['dropDecoder'] 'Miso.Event.Types.DataTransfer' — @drop@ \/ @dragover@ \/ @dragenter@ (@event.dataTransfer@)
-- ['filesDecoder'] @['Miso.Event.Types.FileInfo']@ — @change@ on @\<input type=file\>@
-- ['boundingClientRectDecoder'] 'Miso.Event.Types.DOMRect' — the element's @getBoundingClientRect()@
-- ['modifierStateDecoder'] 'Bool' — @event.getModifierState(key)@, e.g. @\"CapsLock\"@
-- ['selectionTextDecoder'] 'Miso.String.MisoString' — @getSelection().toString()@
-- ['inputSelectionDecoder'] @('Int', 'Int')@ — @selectionStart@ \/ @selectionEnd@ of a text field
-- ['selectedOptionsDecoder'] @['Miso.String.MisoString']@ — values of a @\<select multiple\>@
--
-- = JSON decoders
--
-- 'at' and 'parseWith' snapshot the focus as JSON (keeping only its string,
-- number and boolean properties) and run a 'Miso.JSON.Parser' on it. This is
-- how decoders were written before 1.15, and still a convenient way to read
-- many plain properties at once:
--
-- @
-- -- (offsetX, offsetY) of a click
-- clickXY :: t'Decoder' (Int, Int)
-- clickXY = 'at' [] $ 'Miso.JSON.withObject' \"click\" $ \\o ->
--   (,) \<$\> o 'Miso.JSON..:' \"offsetX\" \<*\> o 'Miso.JSON..:' \"offsetY\"
-- @
--
-- = See also
--
-- * "Miso.Event" — @on@, @onWithOptions@
-- * "Miso.Html.Event" — the pre-wired event handlers (@onClick@, @onInput@, @onPaste@, …)
-- * "Miso.Event.Types" — payload types ('Miso.Event.Types.KeyCode', 'Miso.Event.Types.DataTransfer', …)
-- * "Miso.JSON" — 'Miso.JSON.Value', 'Miso.JSON.Parser', 'Miso.JSON.withObject', @('Miso.JSON..:')@
-----------------------------------------------------------------------------
module Miso.Event.Decoder
  ( -- ** Types
    Decoder
  , DecodeTarget (..)
    -- ** Running
  , runDecoder
    -- ** Moving the focus
  , member
  , members
  , item
  , invoke
  , atEvent
  , atElement
  , atGlobal
    -- ** Reading the focus
  , fromJS
  , items
  , nullable
  , parseWith
  , at
  , decodeAt
  , failDecoder
    -- ** Decoders
  , emptyDecoder
  , keycodeDecoder
  , keyInfoDecoder
  , checkedDecoder
  , valueDecoder
  , pointerDecoder
  , clipboardTextDecoder
  , clipboardDecoder
  , dropDecoder
  , dataTransferDecoder
  , filesDecoder
  , fileInfoDecoder
  , domRectDecoder
  , boundingClientRectDecoder
  , modifierStateDecoder
  , selectionTextDecoder
  , inputSelectionDecoder
  , selectedOptionsDecoder
  ) where
-----------------------------------------------------------------------------
import           Control.Applicative
import           Control.Exception (SomeException, try)
import           System.IO.Unsafe (unsafePerformIO)
import qualified Control.Monad.Fail as Fail
-----------------------------------------------------------------------------
import           Miso.DSL (JSVal, ToArgs(toArgs), FromJSVal(fromJSVal, fromJSValUnchecked), ToJSVal(toJSVal), (!), call, eval, jsg, jsNull, isNull, isUndefined)
import           Miso.Event.Types
import           Miso.JSON (Value, Parser, parseEither, withObject, (.:))
import           Miso.String (MisoString, ms)
-----------------------------------------------------------------------------
-- | Path (consisting of field names) within the event object where a JSON
-- decoder should be applied. See 'decodeAt'.
data DecodeTarget
  = DecodeTarget [MisoString]
  -- ^ Specify single path within Event object, where a decoder should be applied.
  | DecodeTargets [[MisoString]]
  -- ^ Specify multiple paths withing Event object, where decoding should be attempted. The first path where decoding suceeds is the one taken.
-----------------------------------------------------------------------------
-- | `ToJSVal` instance for t'DecodeTarget'.
instance ToJSVal DecodeTarget where
  toJSVal = \case
    DecodeTarget xs -> toJSVal xs
    DecodeTargets xs -> toJSVal xs
-----------------------------------------------------------------------------
-- | Where a t'Decoder' is looking. The path is only used for error messages
-- (most recent step first).
data Cursor
  = Cursor
  { cursorEvent :: JSVal
  , cursorElement :: JSVal
  , cursorFocus :: JSVal
  , cursorPath :: [MisoString]
  }
-----------------------------------------------------------------------------
-- | Reads a value of type @a@ from a DOM event, during dispatch. See the
-- module documentation.
--
-- The constructor is not exported: decoders are built from 'member',
-- 'invoke', 'fromJS', 'parseWith' and the other combinators here.
newtype Decoder a = Decoder (Cursor -> IO (Either MisoString a))
-----------------------------------------------------------------------------
instance Functor Decoder where
  fmap f (Decoder d) = Decoder (fmap (fmap f) . d)
-----------------------------------------------------------------------------
instance Applicative Decoder where
  pure x = Decoder $ \_ -> pure (Right x)
  Decoder df <*> Decoder dx = Decoder $ \c ->
    df c >>= \case
      Left e -> pure (Left e)
      Right f -> fmap f <$> dx c
-----------------------------------------------------------------------------
instance Monad Decoder where
  Decoder d >>= k = Decoder $ \c ->
    d c >>= \case
      Left e -> pure (Left e)
      Right x -> unDecoder (k x) c
-----------------------------------------------------------------------------
instance Fail.MonadFail Decoder where
  fail = failDecoder . ms
-----------------------------------------------------------------------------
-- | @a \<|\> b@ runs @b@ (from the same focus) when @a@ fails.
instance Alternative Decoder where
  empty = failDecoder "empty"
  Decoder a <|> Decoder b = Decoder $ \c ->
    a c >>= \case
      Left _ -> b c
      r -> pure r
-----------------------------------------------------------------------------
unDecoder :: Decoder a -> Cursor -> IO (Either MisoString a)
unDecoder (Decoder d) = d
-----------------------------------------------------------------------------
-- | Run a t'Decoder' against an event. The second argument is the element
-- that 'atElement' focuses; 'Miso.Event.on' passes the element the handler
-- belongs to, and 'Miso.Subscription.Window.windowSub' passes @window@.
--
-- Returns @'Left' message@ when the decoder fails, where the message says
-- where it failed, e.g. @event.clipboardData.getData(): expected a value, got null or undefined@.
--
-- This is how miso runs decoders; it is exported for tests and for custom
-- event listeners. Run it while the event is being dispatched.
--
-- @since 1.15.0.0
runDecoder
  :: Decoder a
  -- ^ What to read
  -> JSVal
  -- ^ The event
  -> JSVal
  -- ^ The element 'atElement' focuses
  -> IO (Either MisoString a)
runDecoder (Decoder d) event element = do
  result <- tryAny $ d Cursor
    { cursorEvent = event
    , cursorElement = element
    , cursorFocus = event
    , cursorPath = ["event"]
    }
  pure $ case result of
    Left e -> Left ("event: " <> ms (show e))
    Right r -> r
-----------------------------------------------------------------------------
-- Note [Decoding during dispatch]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A Decoder reads the live event, so it has to run while the browser is
-- dispatching it. After dispatch:
--
--   * a DataTransfer (event.clipboardData, event.dataTransfer) is
--     "protected": getData returns "" and files is empty;
--   * preventDefault and stopPropagation do nothing;
--   * the layout, the selection, the focused element, ... may have changed.
--
-- So 'Miso.Event.onWithOptions' and 'Miso.Subscription.Window.windowSubWithOptions'
-- install *synchronous* callbacks and call 'runDecoder' in them. (Before
-- 1.15 they were asynchronous: on the wasm and JS backends an asynchronous
-- callback is not guaranteed to finish before dispatch does; on wasm this
-- also made preventDefault from a 'windowSubWithOptions' unreliable.)
-- A synchronous callback must not
-- block, and nothing on that path does: running a Decoder only reads
-- JavaScript values, and a component's sink only enqueues the action and
-- does a non-blocking 'tryPutMVar' to wake the scheduler. The action itself
-- is processed later by the scheduler thread, as before.
--
-- The main-thread (MTS) dispatcher of miso-native already ran synchronously.
-----------------------------------------------------------------------------
tryAny :: IO a -> IO (Either SomeException a)
tryAny = try
-----------------------------------------------------------------------------
isNullish :: JSVal -> IO Bool
isNullish v = (||) <$> isNull v <*> isUndefined v
-----------------------------------------------------------------------------
-- | Where the cursor is, for error messages, e.g. @event.target.files[0]@.
location :: Cursor -> MisoString
location = mconcat . reverse . cursorPath
-----------------------------------------------------------------------------
failAt :: Cursor -> MisoString -> IO (Either MisoString a)
failAt c msg = pure (Left (location c <> ": " <> msg))
-----------------------------------------------------------------------------
-- | Fail with a message. Same as 'fail', at 'MisoString'.
--
-- @
-- positive :: t'Decoder' Int
-- positive = 'member' \"detail\" 'fromJS' >>= \\n ->
--   if n > 0 then pure n else 'failDecoder' \"expected a positive detail\"
-- @
--
-- @since 1.15.0.0
failDecoder :: MisoString -> Decoder a
failDecoder msg = Decoder $ \c -> failAt c msg
-----------------------------------------------------------------------------
-- | Move the focus to a value computed from the current focus, then run a
-- decoder there. Fails, instead of throwing, when the focus is
-- @null@ \/ @undefined@ or when JavaScript throws.
focusWith
  :: MisoString
  -- ^ The step, for error messages (e.g. @\".target\"@)
  -> (JSVal -> IO (Either MisoString JSVal))
  -> Decoder a
  -> Decoder a
focusWith step get (Decoder d) = Decoder $ \c -> do
  let next v = c { cursorFocus = v, cursorPath = step : cursorPath c }
  nullish <- isNullish (cursorFocus c)
  if nullish
    then failAt c ("cannot read " <> step <> " of null or undefined")
    else tryAny (get (cursorFocus c)) >>= \case
      Left e -> failAt (next (cursorFocus c)) ("threw " <> ms (show e))
      Right (Left msg) -> failAt (next (cursorFocus c)) msg
      Right (Right v) -> d (next v)
-----------------------------------------------------------------------------
-- | @safeGet o k null@ reads @o[k]@; @safeGet o m args@ calls @o.m(...args)@.
--
-- Both run in JavaScript, inside a @try@: a getter or a method can throw, and
-- not every backend turns a JavaScript exception thrown from an FFI call into
-- a Haskell exception that 'try' can catch (the JS backend doesn't; the
-- exception escapes to the scheduler and kills the thread).
safeGet :: JSVal -> MisoString -> JSVal -> IO (Either MisoString JSVal)
safeGet o k args = do
  r <- call safeGetFunction jsNull (o, k, args)
  err <- r ! "error"
  ok <- isUndefined err
  if ok
    then Right <$> r ! "value"
    else Left <$> fromJSValUnchecked err
-----------------------------------------------------------------------------
-- | The JavaScript half of 'safeGet', created once.
safeGetFunction :: JSVal
safeGetFunction = unsafePerformIO $ eval $ mconcat
  [ "(function (o, k, args) {"
  , "  try {"
  , "    if (args === null) return { value: o[k] };"
  , "    var f = o[k];"
  , "    if (typeof f !== 'function') return { error: 'no such method' };"
  , "    return { value: f.apply(o, args) };"
  , "  } catch (e) {"
  , "    return { error: 'threw ' + String(e) };"
  , "  }"
  , "})"
  ]
{-# NOINLINE safeGetFunction #-}
-----------------------------------------------------------------------------
-- | Focus the property @k@ of the current focus (@focus[k]@), and run a
-- decoder there. Inherited properties and getters work (e.g. @\"target\"@,
-- @\"clipboardData\"@, @\"width\"@ of a @DOMRect@).
--
-- @
-- -- event.target.value
-- 'member' \"target\" ('member' \"value\" 'fromJS') :: t'Decoder' 'MisoString'
-- @
--
-- A missing property is @undefined@, which 'fromJS' rejects and 'nullable'
-- accepts. Reading a property of @null@ or @undefined@ fails.
--
-- @since 1.15.0.0
member :: MisoString -> Decoder a -> Decoder a
member k = focusWith ("." <> k) (\o -> safeGet o k jsNull)
-----------------------------------------------------------------------------
-- | Focus a path of properties.
--
-- @
-- 'members' [\"target\", \"validity\", \"valid\"] 'fromJS' :: t'Decoder' Bool
-- @
--
-- @since 1.15.0.0
members :: [MisoString] -> Decoder a -> Decoder a
members ks d = foldr member d ks
-----------------------------------------------------------------------------
-- | Focus element @i@ (from 0) of an array or array-like, e.g.
-- @event.changedTouches[0]@:
--
-- @
-- 'member' \"changedTouches\" ('item' 0 ('member' \"clientX\" 'fromJS')) :: t'Decoder' Double
-- @
--
-- An index past the end is @undefined@. To decode every element, use 'items'.
--
-- @since 1.15.0.0
item :: Int -> Decoder a -> Decoder a
item i = focusWith ("[" <> ms i <> "]") (\o -> safeGet o (ms i) jsNull)
-----------------------------------------------------------------------------
-- | Call the method @m@ of the focus with @args@ (see 'Miso.DSL.ToArgs':
-- @()@ for no arguments, a value for one, a tuple or list for several), and
-- focus its result.
--
-- @
-- -- event.clipboardData.getData(\"text\/html\")
-- 'member' \"clipboardData\" ('invoke' \"getData\" (\"text\/html\" :: 'MisoString') 'fromJS')
--
-- -- event.target.closest(\"li\").id
-- 'member' \"target\" ('invoke' \"closest\" (\"li\" :: 'MisoString') ('member' \"id\" 'fromJS'))
--
-- -- event.composedPath().length
-- 'invoke' \"composedPath\" () ('member' \"length\" 'fromJS') :: t'Decoder' Int
-- @
--
-- Fails if the focus has no such method (so @\<|\>@ can fall back where a
-- browser lacks it) or if the call throws. The method is called with the
-- focus as @this@.
--
-- Use it for methods that read; see "Methods should only read" in the
-- module documentation.
--
-- @since 1.15.0.0
invoke :: ToArgs args => MisoString -> args -> Decoder a -> Decoder a
invoke m args = focusWith ("." <> m <> "()") $ \o ->
  safeGet o m =<< toJSVal =<< toArgs args
-----------------------------------------------------------------------------
-- | Focus the event again, from anywhere in a decoder. A t'Decoder' starts
-- at the event, so this is only needed after moving the focus elsewhere:
--
-- @
-- 'atElement' ('member' \"id\" 'fromJS' >>= \\i -> 'atEvent' ('member' \"type\" 'fromJS'))
-- @
--
-- @since 1.15.0.0
atEvent :: Decoder a -> Decoder a
atEvent (Decoder d) = Decoder $ \c ->
  d c { cursorFocus = cursorEvent c, cursorPath = ["event"] }
-----------------------------------------------------------------------------
-- | Focus the element whose handler is running: the element in your view
-- that has the @on@ attribute, which is also the 'Miso.Types.DOMRef' given
-- to the handler. (miso delegates events, so @event.currentTarget@ is the
-- mount point rather than this element.)
--
-- @
-- -- this element's size and position, as the event happens
-- 'atElement' ('invoke' \"getBoundingClientRect\" () 'domRectDecoder')
--
-- -- how far this scrollable element is scrolled
-- 'atElement' ('member' \"scrollTop\" 'fromJS') :: t'Decoder' Double
-- @
--
-- In a 'Miso.Subscription.Window.windowSub' it focuses @window@.
--
-- @since 1.15.0.0
atElement :: Decoder a -> Decoder a
atElement (Decoder d) = Decoder $ \c ->
  d c { cursorFocus = cursorElement c, cursorPath = ["element"] }
-----------------------------------------------------------------------------
-- | Focus @globalThis@ (@window@ in a browser), to read browser state as
-- the event happens:
--
-- @
-- -- the selected text, e.g. on mouseup
-- 'atGlobal' ('invoke' \"getSelection\" () ('invoke' \"toString\" () 'fromJS')) :: t'Decoder' 'MisoString'
--
-- -- the id of the element that has focus
-- 'atGlobal' ('members' [\"document\", \"activeElement\", \"id\"] 'fromJS') :: t'Decoder' 'MisoString'
-- @
--
-- @since 1.15.0.0
atGlobal :: Decoder a -> Decoder a
atGlobal (Decoder d) = Decoder $ \c -> do
  g <- jsg "globalThis"
  d c { cursorFocus = g, cursorPath = ["globalThis"] }
-----------------------------------------------------------------------------
-- | Convert the focus with its 'FromJSVal' instance.
--
-- @
-- 'member' \"clientX\" 'fromJS' :: t'Decoder' Double
-- 'member' \"key\"     'fromJS' :: t'Decoder' 'MisoString'
-- 'member' \"target\"  'fromJS' :: t'Decoder' 'JSVal'    -- the element itself
-- @
--
-- Fails on @null@ and @undefined@; use @'nullable' 'fromJS'@ to accept them.
-- See "Reading the focus with fromJS" in the module documentation.
--
-- @since 1.15.0.0
fromJS :: FromJSVal a => Decoder a
fromJS = Decoder $ \c -> do
  let v = cursorFocus c
  nullish <- isNullish v
  if nullish
    then failAt c "expected a value, got null or undefined"
    else tryAny (fromJSVal v) >>= \case
      Left e -> failAt c ("threw " <> ms (show e))
      Right Nothing -> failAt c "could not convert this value"
      Right (Just x) -> pure (Right x)
-----------------------------------------------------------------------------
-- | Decode every element of the focus, which must be an array or have a
-- @length@ and numbered elements: @Array@, @FileList@, @TouchList@,
-- @NodeList@, @HTMLCollection@, @DataTransferItemList@, @DOMStringList@, ...
--
-- @
-- -- the names of the files dropped
-- 'member' \"dataTransfer\" ('member' \"files\" ('items' ('member' \"name\" 'fromJS'))) :: t'Decoder' ['MisoString']
--
-- -- the tag names of the elements the event went through
-- 'invoke' \"composedPath\" () ('items' ('member' \"nodeName\" 'fromJS' \<|\> pure \"#window\"))
-- @
--
-- @since 1.15.0.0
items :: Decoder a -> Decoder [a]
items d = do
  n <- member "length" fromJS
  traverse (`item` d) [0 .. n - 1 :: Int]
-----------------------------------------------------------------------------
-- | 'Nothing' when the focus is @null@ or @undefined@, otherwise run the
-- decoder.
--
-- @
-- -- event.relatedTarget is null when the pointer came from outside the window
-- 'member' \"relatedTarget\" ('nullable' ('member' \"id\" 'fromJS')) :: t'Decoder' (Maybe 'MisoString')
-- @
--
-- @since 1.15.0.0
nullable :: Decoder a -> Decoder (Maybe a)
nullable (Decoder d) = Decoder $ \c -> do
  nullish <- isNullish (cursorFocus c)
  if nullish
    then pure (Right Nothing)
    else fmap Just <$> d c
-----------------------------------------------------------------------------
-- | Snapshot the focus as a JSON 'Value' and parse it. The snapshot keeps
-- the focus's own and inherited string, number and boolean properties
-- (objects and functions are left out); an array-like becomes an array of
-- snapshots; @null@ and @undefined@ become @null@.
--
-- @
-- 'member' \"detail\" ('parseWith' 'Miso.JSON.parseJSON') :: t'Decoder' MyDetail
-- @
--
-- Same as @'at' []@.
--
-- @since 1.15.0.0
parseWith :: (Value -> Parser a) -> Decoder a
parseWith = at []
-----------------------------------------------------------------------------
-- | Snapshot the value at a path below the focus (see 'parseWith') and
-- parse it. This is how decoders were written before 1.15:
--
-- @
-- 'valueDecoder' = 'at' [\"target\"] $ 'withObject' \"target\" $ \\o -> o '.:' \"value\"
-- @
--
-- Unlike 'members', a path that runs into @null@ or @undefined@ gives the
-- parser @null@ instead of failing.
at
  :: [MisoString]
  -- ^ Path below the focus (e.g. @[\"target\"]@ for @event.target@, @[]@ for the focus itself)
  -> (Value -> Parser a)
  -- ^ JSON parser applied to the snapshot at that path
  -> Decoder a
at path parser = Decoder $ \c ->
  tryAny (snapshot path (cursorFocus c)) >>= \case
    Left e -> failAt c ("threw " <> ms (show e))
    Right Nothing -> failAt c "could not convert this value to JSON"
    Right (Just v) ->
      case parseEither parser v of
        Left msg -> failAt c msg
        Right x -> pure (Right x)
-----------------------------------------------------------------------------
-- | JSON snapshot of the value at @path@ below @v@, by @miso.eventJSON@
-- (see 'Miso.FFI.Internal.eventJSON'). That only takes objects (it uses @in@), and throws
-- on a primitive, which is then converted directly.
snapshot :: [MisoString] -> JSVal -> IO (Maybe Value)
snapshot path v = do
  pathVal <- toJSVal path
  miso <- jsg "miso"
  args <- toJSVal [pathVal, v]
  safeGet miso "eventJSON" args >>= \case
    Right json -> fromJSVal json
    Left _ | null path -> fromJSVal v
           | otherwise -> pure Nothing
-----------------------------------------------------------------------------
-- | A JSON decoder at a t'DecodeTarget'. For 'DecodeTargets', the first
-- path whose snapshot parses wins.
--
-- @
-- 'decodeAt' ('DecodeTarget' [\"target\"]) parser = 'at' [\"target\"] parser
-- @
--
-- @since 1.15.0.0
decodeAt :: DecodeTarget -> (Value -> Parser a) -> Decoder a
decodeAt target parser =
  case target of
    DecodeTarget path -> at path parser
    DecodeTargets paths ->
      foldr (\path rest -> at path parser <|> rest)
        (failDecoder "no DecodeTarget matched") paths
-----------------------------------------------------------------------------
-- | Empty t'Decoder' for use with events like "click" that do not
-- return any meaningful values
emptyDecoder :: Decoder ()
emptyDecoder = pure ()
-----------------------------------------------------------------------------
-- | Retrieves either "keyCode", "which" or "charCode" field in t'Decoder'
keycodeDecoder :: Decoder KeyCode
keycodeDecoder = at [] $ withObject "event" $ \o ->
  KeyCode <$> (o .: "keyCode" <|> o .: "which" <|> o .: "charCode")
-----------------------------------------------------------------------------
-- | Retrieves either "keyCode", "which" or "charCode" field in t'Decoder',
-- along with shift, ctrl, meta and alt.
keyInfoDecoder :: Decoder KeyInfo
keyInfoDecoder = at [] $ withObject "event" $ \o ->
  KeyInfo
    <$> (o .: "keyCode" <|> o .: "which" <|> o .: "charCode")
    <*> o .: "shiftKey"
    <*> o .: "metaKey"
    <*> o .: "ctrlKey"
    <*> o .: "altKey"
-----------------------------------------------------------------------------
-- | Retrieves "value" field in t'Decoder'
valueDecoder :: Decoder MisoString
valueDecoder = at ["target"] $ withObject "target" $ \o -> o .: "value"
-----------------------------------------------------------------------------
-- | Retrieves "checked" field in t'Decoder'
checkedDecoder :: Decoder Checked
checkedDecoder = at ["target"] $ withObject "target" $ \o ->
  Checked <$> (o .: "checked")
-----------------------------------------------------------------------------
-- | Pointer t'Decoder' for use with events like "onpointerover"
pointerDecoder :: Decoder PointerEvent
pointerDecoder = at [] $ withObject "pointerDecoder" $ \o ->
  PointerEvent
    <$> o .: "pointerType"
    <*> o .: "pointerId"
    <*> o .: "isPrimary"
    <*> pair o "clientX" "clientY"
    <*> pair o "screenX" "screenY"
    <*> pair o "offsetX" "offsetY"
    <*> pair o "pageX" "pageY"
    <*> pair o "tiltX" "tiltY"
    <*> o .: "pressure"
    <*> o .: "button"
  where
    pair o x y = liftA2 (,) (o .: x) (o .: y)
-----------------------------------------------------------------------------
-- | The pasted plain text, for @paste@:
-- @event.clipboardData.getData(\"text\/plain\")@ (@\"\"@ when nothing textual
-- was pasted, e.g. only an image).
--
-- @
-- 'Miso.Event.on' \"paste\" 'clipboardTextDecoder' (\\txt _ _ -> Pasted txt)
-- @
--
-- See also 'Miso.Html.Event.onPaste'. Add 'Miso.Event.Types.clipboardEvents'
-- to your component's @events@.
--
-- @since 1.15.0.0
clipboardTextDecoder :: Decoder MisoString
clipboardTextDecoder =
  member "clipboardData" (invoke "getData" ("text/plain" :: MisoString) fromJS)
-----------------------------------------------------------------------------
-- | Everything on the clipboard, for @paste@ (and @copy@ \/ @cut@, where it
-- is what a previous handler put there): every format with its data, and
-- the pasted files. See 'dataTransferDecoder'.
--
-- @since 1.15.0.0
clipboardDecoder :: Decoder DataTransfer
clipboardDecoder = member "clipboardData" dataTransferDecoder
-----------------------------------------------------------------------------
-- | What is being dragged or dropped, for @drop@ \/ @dragover@ \/
-- @dragenter@ \/ @dragstart@: @event.dataTransfer@. See 'dataTransferDecoder'.
--
-- During @dragover@ and @dragenter@ the browser hides the data itself
-- ('Miso.Event.Types.dataTransferData' has empty strings) but shows the
-- formats and the files' types; @drop@ has everything. For a drop to be
-- allowed, the @dragover@ handler must prevent its default (see
-- 'Miso.Html.Event.onDropData').
--
-- @since 1.15.0.0
dropDecoder :: Decoder DataTransfer
dropDecoder = member "dataTransfer" dataTransferDecoder
-----------------------------------------------------------------------------
-- | Decode the focus as a
-- <https://developer.mozilla.org/en-US/docs/Web/API/DataTransfer DataTransfer>:
-- its @types@, @getData(type)@ for each type other than @\"Files\"@, its
-- @files@ (see 'fileInfoDecoder'), @dropEffect@ and @effectAllowed@.
--
-- @since 1.15.0.0
dataTransferDecoder :: Decoder DataTransfer
dataTransferDecoder = do
  types <- member "types" (items fromJS)
  datas <- traverse dataOf (filter (/= "Files") types)
  files <- member "files" (items fileInfoDecoder) <|> pure []
  dropEffect <- member "dropEffect" fromJS <|> pure "none"
  effectAllowed <- member "effectAllowed" fromJS <|> pure "uninitialized"
  pure (DataTransfer types datas files dropEffect effectAllowed)
-----------------------------------------------------------------------------
-- | @(format, focus.getData(format))@
dataOf :: MisoString -> Decoder (MisoString, MisoString)
dataOf format = (,) format <$> invoke "getData" format fromJS
-----------------------------------------------------------------------------
-- | The files chosen in an @\<input type=file\>@, for @change@:
-- @event.target.files@.
--
-- @
-- 'Miso.Html.Element.input_' [ 'Miso.Html.Property.type_' \"file\", 'Miso.Html.Property.multiple_' True, 'Miso.Event.on' \"change\" 'filesDecoder' (\\fs _ _ -> Chose fs) ]
-- @
--
-- To read a file's contents, decode the @File@ objects themselves
-- (@'members' [\"target\", \"files\"] ('items' 'fromJS') :: t'Decoder' ['JSVal']@)
-- and read them in @update@.
--
-- @since 1.15.0.0
filesDecoder :: Decoder [FileInfo]
filesDecoder = members ["target", "files"] (items fileInfoDecoder)
-----------------------------------------------------------------------------
-- | Decode the focus as a
-- <https://developer.mozilla.org/en-US/docs/Web/API/File File>: its name,
-- type, size and modification time.
--
-- @since 1.15.0.0
fileInfoDecoder :: Decoder FileInfo
fileInfoDecoder =
  FileInfo
    <$> member "name" fromJS
    <*> member "type" fromJS
    <*> member "size" fromJS
    <*> member "lastModified" fromJS
-----------------------------------------------------------------------------
-- | Decode the focus as a
-- <https://developer.mozilla.org/en-US/docs/Web/API/DOMRect DOMRect>, e.g.
-- the result of @getBoundingClientRect()@ or an item of @getClientRects()@.
--
-- @since 1.15.0.0
domRectDecoder :: Decoder DOMRect
domRectDecoder =
  DOMRect
    <$> member "x" fromJS
    <*> member "y" fromJS
    <*> member "width" fromJS
    <*> member "height" fromJS
    <*> member "top" fromJS
    <*> member "right" fromJS
    <*> member "bottom" fromJS
    <*> member "left" fromJS
-----------------------------------------------------------------------------
-- | The handler's element's @getBoundingClientRect()@, measured as the
-- event happens (see 'atElement').
--
-- @
-- -- where on the element the click landed, from 0 to 1
-- clickFraction :: t'Decoder' Double
-- clickFraction = do
--   rect <- 'boundingClientRectDecoder'
--   x <- 'member' \"clientX\" 'fromJS'
--   pure ((x - 'rectLeft' rect) \/ 'rectWidth' rect)
-- @
--
-- @since 1.15.0.0
boundingClientRectDecoder :: Decoder DOMRect
boundingClientRectDecoder =
  atElement (invoke "getBoundingClientRect" () domRectDecoder)
-----------------------------------------------------------------------------
-- | @event.getModifierState(key)@, for keyboard, mouse and pointer events:
-- whether a modifier or lock key is on, e.g. @\"CapsLock\"@, @\"NumLock\"@,
-- @\"Shift\"@, @\"AltGraph\"@. See
-- <https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/getModifierState getModifierState>.
--
-- @
-- 'Miso.Event.on' \"keyup\" ('modifierStateDecoder' \"CapsLock\") (\\on _ _ -> CapsLock on)
-- @
--
-- @since 1.15.0.0
modifierStateDecoder :: MisoString -> Decoder Bool
modifierStateDecoder key = invoke "getModifierState" key fromJS
-----------------------------------------------------------------------------
-- | The text the user has selected in the page, as the event happens:
-- @window.getSelection().toString()@. Useful on @mouseup@ and @keyup@.
-- (For the selection inside an @\<input\>@ or @\<textarea\>@, use
-- 'inputSelectionDecoder'.)
--
-- @since 1.15.0.0
selectionTextDecoder :: Decoder MisoString
selectionTextDecoder =
  atGlobal (invoke "getSelection" () (invoke "toString" () fromJS))
-----------------------------------------------------------------------------
-- | @(selectionStart, selectionEnd)@ of the @\<input\>@ or @\<textarea\>@
-- the event happened in: the caret position when they are equal. For
-- @select@, @keyup@, @click@ and @input@. Fails for inputs that have no
-- text selection (e.g. @type=number@).
--
-- @since 1.15.0.0
inputSelectionDecoder :: Decoder (Int, Int)
inputSelectionDecoder =
  member "target" $
    (,) <$> member "selectionStart" fromJS
        <*> member "selectionEnd" fromJS
-----------------------------------------------------------------------------
-- | The values of the selected options of a @\<select multiple\>@, for
-- @change@: @event.target.selectedOptions@. ('valueDecoder' gives only the
-- first one.)
--
-- @since 1.15.0.0
selectedOptionsDecoder :: Decoder [MisoString]
selectedOptionsDecoder =
  members ["target", "selectedOptions"] (items (member "value" fromJS))
-----------------------------------------------------------------------------
