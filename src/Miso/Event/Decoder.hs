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
-- = Reading the event with "Miso.DSL"
--
-- t'Decoder' is an instance of 'MonadJS', so the "Miso.DSL" operators you
-- use in 'IO' work in a t'Decoder' too:
--
-- ['currentEvent']   the event
-- ['currentElement'] the element whose handler is running (see below)
-- [@o '!' k@]          @o[k]@ (a property, own or inherited, including getters)
-- [@o '!!' i@]         @o[i]@ of an array or array-like
-- [@(o '#' m) args@]   @o.m(...args)@
-- [@'global' '!' k@]   @globalThis[k]@
--
-- and these turn a t'JSVal' into a Haskell value:
--
-- ['fromJS']      convert with 'Miso.DSL.FromJSVal' ('MisoString', 'Int', 'Double', 'Bool', 'JSVal', ...)
-- ['parsed']      snapshot as JSON and convert with 'FromJSON'
-- ['parseWith']   snapshot as JSON and run a 'Miso.JSON.Parser' on it
-- ['items']       decode every element of an array-like (@Array@, @FileList@, @TouchList@, @NodeList@, ...)
-- ['nullable']    'Nothing' for @null@ or @undefined@
--
-- t'Decoder' is a 'Monad' and an 'Alternative': combine reads with
-- @\<$\>@ \/ @\<*\>@ \/ @do@, and give fallbacks with @\<|\>@. Any failure
-- makes the decoder fail, and @\<|\>@ moves on to the next choice. Unlike in
-- 'IO', these are failures and not exceptions:
--
-- * reading a property of @null@ or @undefined@;
-- * calling a method that doesn't exist (e.g. one this browser lacks);
-- * a JavaScript exception thrown by a getter or a method.
--
-- @
-- -- event.clipboardData.getData(\"text\/plain\")
-- pastedText :: t'Decoder' 'MisoString'
-- pastedText = do
--   clipboard <- 'currentEvent' >>= ('!' \"clipboardData\")
--   'fromJS' =<< (clipboard '#' \"getData\") (\"text\/plain\" :: 'MisoString')
--
-- -- Where the click landed, relative to the element that was clicked
-- clickInElement :: t'Decoder' (Double, Double)
-- clickInElement = do
--   rect <- 'boundingClientRectDecoder'
--   e <- 'currentEvent'
--   x <- 'fromJS' =<< e '!' \"clientX\"
--   y <- 'fromJS' =<< e '!' \"clientY\"
--   pure (x - 'rectLeft' rect, y - 'rectTop' rect)
--
-- -- Coalesced pointer events where the browser has them, else just this one
-- coalesced :: t'Decoder' [(Double, Double)]
-- coalesced = do
--   e <- 'currentEvent'
--   ('items' xy =<< (e '#' \"getCoalescedEvents\") ()) \<|\> fmap pure (xy e)
--   where
--     xy p = (,) \<$\> ('fromJS' =<< p '!' \"clientX\") \<*\> ('fromJS' =<< p '!' \"clientY\")
-- @
--
-- == The element
--
-- miso delegates events: the browser's listener is on the mount point, so
-- @event.currentTarget@ is the mount point, not your element.
-- 'currentElement' is the element that the handler was attached to in your
-- 'Miso.Types.View' (the same element as the 'Miso.Types.DOMRef' passed to
-- the handler). @event.target@ is still the innermost element the event
-- happened on, which may be a child of yours.
--
-- == Methods should only read
--
-- '#' calls whatever method you name. Use it for methods that read
-- (@getData@, @getBoundingClientRect@, @getModifierState@, @closest@,
-- @composedPath@, @matches@, ...). A method with an effect, such as
-- @preventDefault@ or @remove@, runs every time the event fires and is not
-- what a decoder is for: use 'Miso.Event.Types.Options' for
-- @preventDefault@ \/ @stopPropagation@, and effects in @update@ for the rest.
--
-- == Reading values with 'fromJS'
--
-- 'fromJS' uses the 'Miso.DSL.FromJSVal' instance of the result type. It
-- fails on @null@ and @undefined@ (use 'nullable' to accept them). Some
-- backends convert without checking the JavaScript type, so reading a
-- number as a 'MisoString' is not guaranteed to fail: read each value at
-- its real type, or use 'parsed', which checks.
--
-- 'fromJS' at type 'JSVal' is the value itself, e.g. a @File@ to read later
-- in @update@. Only keep a 'JSVal' that stays valid after dispatch (a
-- @File@ or an element do; a @DataTransfer@ does not).
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
-- 'at' and 'parseWith' snapshot a value as JSON (keeping only its string,
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
-- * "Miso.DSL" — 'MonadJS', '!', '!!', '#', 'global'
-- * "Miso.JSON" — 'Miso.JSON.Value', 'Miso.JSON.Parser', 'Miso.JSON.withObject', @('Miso.JSON..:')@
-----------------------------------------------------------------------------
module Miso.Event.Decoder
  ( -- ** Types
    Decoder
  , DecodeTarget (..)
    -- ** Running
  , runDecoder
    -- ** The event and the element
  , currentEvent
  , currentElement
    -- ** Reading values
  , MonadJS (..)
  , global
  , fromJS
  , items
  , nullable
  , parsed
  , parseWith
  , parseFields
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
import           Prelude hiding ((!!))
import           Control.Applicative
import           Control.Exception (SomeException, evaluate, try)
import qualified Control.Monad.Fail as Fail
-----------------------------------------------------------------------------
import           Miso.DSL (JSVal, MonadJS(..), ToArgs(toArgs), FromJSVal(fromJSVal, fromJSValUnchecked), ToJSVal(toJSVal), ToObject(toObject), call, freeJSVal, global, jsg, jsNull, isNull, isUndefined)
import           Miso.Event.Types
import           Miso.JSON (FromJSON(parseJSON), Value(Null, String), Parser, parseEither, withObject, (.:))
import           Miso.String (MisoString, ms)
import           System.IO.Unsafe (unsafePerformIO)
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
-- | What a t'Decoder' runs against: the event, and the element whose
-- handler is running.
--
-- The fields are strict: see Note [Evaluated JSVals in argument lists].
data Env
  = Env
  { envEvent :: !JSVal
  , envElement :: !JSVal
  }
-----------------------------------------------------------------------------
-- | Reads a value of type @a@ from a DOM event, during dispatch. See the
-- module documentation.
--
-- The constructor is not exported: decoders are built from 'currentEvent',
-- the 'MonadJS' operators, 'fromJS' and the other combinators here.
newtype Decoder a = Decoder (Env -> IO (Either MisoString a))
-----------------------------------------------------------------------------
instance Functor Decoder where
  fmap f (Decoder d) = Decoder (fmap (fmap f) . d)
-----------------------------------------------------------------------------
instance Applicative Decoder where
  pure x = Decoder $ \_ -> pure (Right x)
  Decoder df <*> Decoder dx = Decoder $ \env ->
    df env >>= \case
      Left e -> pure (Left e)
      Right f -> fmap f <$> dx env
-----------------------------------------------------------------------------
instance Monad Decoder where
  Decoder d >>= k = Decoder $ \env ->
    d env >>= \case
      Left e -> pure (Left e)
      Right x -> unDecoder (k x) env
-----------------------------------------------------------------------------
instance Fail.MonadFail Decoder where
  fail = failDecoder . ms
-----------------------------------------------------------------------------
-- | @a \<|\> b@ runs @b@ when @a@ fails.
--
-- 'some' and 'many' always fail. A t'Decoder' reads without consuming
-- anything, so @'many' d@ would run @d@ forever as long as it succeeds
-- (inside the browser's event dispatch). To decode every element of an
-- array, use 'items'.
instance Alternative Decoder where
  empty = failDecoder "empty"
  Decoder a <|> Decoder b = Decoder $ \env ->
    a env >>= \case
      Left _ -> b env
      r -> pure r
  some _ = failDecoder "some: a Decoder consumes nothing, so some would not end; use items"
  many _ = failDecoder "many: a Decoder consumes nothing, so many would not end; use items"
-----------------------------------------------------------------------------
-- | Every step goes through @miso.decoderStep@, so a JavaScript exception,
-- a read from @null@ \/ @undefined@ or a missing method is a decoder
-- failure, which @\<|\>@ can recover from. See Note [Decoding during
-- dispatch].
instance MonadJS Decoder where
  o ! k = stepTo ("." <> k) k Nothing o
  o !! i = stepTo ("[" <> ms i <> "]") (ms i) Nothing o
  (#) o m args = stepTo ("." <> m <> "()") m (Just (toJSVal =<< toArgs args)) o
-----------------------------------------------------------------------------
unDecoder :: Decoder a -> Env -> IO (Either MisoString a)
unDecoder (Decoder d) = d
-----------------------------------------------------------------------------
-- | Run a t'Decoder' against an event. The second argument is the element
-- that 'currentElement' returns; 'Miso.Event.on' passes the element the
-- handler belongs to, and 'Miso.Subscription.Window.windowSub' passes
-- @window@.
--
-- Returns @'Left' message@ when the decoder fails, where the message says
-- which step failed, e.g. @.getData(): no such method@.
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
  -- ^ The element 'currentElement' returns
  -> IO (Either MisoString a)
runDecoder (Decoder d) event element = do
  result <- tryAny (d (Env event element))
  pure $ case result of
    Left e -> Left ("threw " <> ms (show e))
    Right r -> r
-----------------------------------------------------------------------------
-- Note [Decoding during dispatch]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A Decoder reads the live event, so it has to run while the browser is
-- dispatching it. After dispatch:
--
--   * a DataTransfer (event.clipboardData, event.dataTransfer) is
--     "protected": getData returns "" and files is empty;
--   * the layout, the selection, the focused element, ... may have changed.
--
-- 'Miso.Event.onWithOptions', 'Miso.Subscription.Window.windowSubWithOptions'
-- and the miso-native main-thread dispatcher call 'runDecoder' from a
-- synchronous callback; see Note [Synchronous event callbacks] in
-- "Miso.Event". Running a Decoder only reads JavaScript values, so it never
-- blocks.
--
-- Each '!', '!!' and '#' in a Decoder is one call to @miso.decoderStep@ (in
-- ts/miso/event.ts), which never throws: a getter or a method can throw, and
-- not every backend turns a JavaScript exception raised inside an FFI call
-- into a Haskell exception that 'try' can catch (the JS backend doesn't; the
-- exception escapes to the scheduler and kills the thread). It lives in the
-- miso bundle, not in an @eval@ string, so decoders work under a
-- Content-Security-Policy without @'unsafe-eval'@.
--
-- A failed step returns the one shared value @miso.decoderFailed@ (the
-- reason is in @miso.decoderError()@), so telling success from failure is a
-- single @===@. 'stepFunctions' looks these up once, not per event: the miso
-- bundle is part of the program (see 'Miso.Runtime'), so they don't change.
--
-- A step frees the handles it allocates (argument arrays, the error
-- object), as the rest of the runtime does on hot paths. It cannot free the
-- value it steps to: the decoder may return it ('fromJS' at 'JSVal'). Nor
-- can it free a JSON result that is a string: on WASM the 'MisoString' in
-- @'Miso.JSON.String'@ shares its handle (see 'jsonStep').
-----------------------------------------------------------------------------
-- Note [Evaluated JSVals in argument lists]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A step passes the value it reads from to @miso.decoderStep@ inside an
-- argument list ('call' with a tuple, 'toJSVal' of a list). On the JS
-- backend, a 'JSVal' in such a list must already be evaluated: an
-- unevaluated thunk reaches JavaScript as a heap object, not the value
-- (@global ! \"document\"@ read @undefined@ from it).
--
-- The value comes from the user's code (the receiver of '!', '!!', '#', or
-- the argument of 'parseWith' and 'parseFields'), so every step 'evaluate's
-- it before building the argument list. The fields of 'Env' and
-- 'StepFunctions' are strict for the same reason.
-----------------------------------------------------------------------------
tryAny :: IO a -> IO (Either SomeException a)
tryAny = try
-----------------------------------------------------------------------------
isNullish :: JSVal -> IO Bool
isNullish v = (||) <$> isNull v <*> isUndefined v
-----------------------------------------------------------------------------
-- | Fail with a message. Same as 'fail', at 'MisoString'.
--
-- @
-- positive :: t'Decoder' Int
-- positive = do
--   n <- 'currentEvent' >>= ('!' \"detail\") >>= 'fromJS'
--   if n > 0 then pure n else 'failDecoder' \"expected a positive detail\"
-- @
--
-- @since 1.15.0.0
failDecoder :: MisoString -> Decoder a
failDecoder msg = Decoder $ \_ -> pure (Left msg)
-----------------------------------------------------------------------------
-- | The decoder helpers in the miso bundle (see Note [Decoding during
-- dispatch]), looked up once.
data StepFunctions
  = StepFunctions
  { jsMiso :: !JSVal
  , jsStep :: !JSVal
  , jsFailed :: !JSVal
  , jsError :: !JSVal
  }
-----------------------------------------------------------------------------
stepFunctions :: StepFunctions
stepFunctions = unsafePerformIO $ do
  miso <- jsg "miso"
  StepFunctions miso
    <$> miso ! "decoderStep"
    <*> miso ! "decoderFailed"
    <*> miso ! "decoderError"
{-# NOINLINE stepFunctions #-}
-----------------------------------------------------------------------------
-- | The outcome of one @miso.decoderStep@.
data Step
  = StepOk JSVal
  | StepNullish
  -- ^ The step started from @null@ or @undefined@
  | StepFailed MisoString
-----------------------------------------------------------------------------
-- | @step o k Nothing@ reads @o[k]@; @step o m (Just args)@ calls
-- @o.m(...args)@, where @args@ is a JavaScript array, which 'step' frees.
step :: JSVal -> MisoString -> Maybe JSVal -> IO Step
step o k margs = do
  let js = stepFunctions
  r <- call (jsStep js) jsNull (o, k, maybe jsNull id margs)
  mapM_ freeJSVal margs
  if r /= jsFailed js
    then pure (StepOk r)
    else do
      err <- call (jsError js) jsNull ()
      nullish <- fromJSValUnchecked =<< err ! "nullish"
      result <-
        if nullish
          then pure StepNullish
          else StepFailed <$> (fromJSValUnchecked =<< err ! "message")
      freeJSVal err
      pure result
-----------------------------------------------------------------------------
-- | One @miso.decoderStep@ from @o@, as a t'Decoder'. Fails, instead of
-- throwing, when @o@ is @null@ \/ @undefined@ or when JavaScript throws.
stepTo
  :: ToObject o
  => MisoString
  -- ^ The step, for error messages (e.g. @\".target\"@)
  -> MisoString
  -- ^ The property or method name
  -> Maybe (IO JSVal)
  -- ^ 'Nothing' to read the property, or the arguments to call the method with
  -> o
  -> Decoder JSVal
stepTo label key getArgs o = Decoder $ \_ -> do
  result <- tryAny $ do
    v <- evaluate =<< toJSVal =<< toObject o
    step v key =<< sequence getArgs
  pure $ case result of
    Right (StepOk v) -> Right v
    Right StepNullish -> Left ("cannot read " <> label <> " of null or undefined")
    Right (StepFailed msg) -> Left (label <> ": " <> msg)
    Left e -> Left (label <> ": threw " <> ms (show e))
-----------------------------------------------------------------------------
-- | The event being dispatched.
--
-- @
-- -- event.target.value
-- 'currentEvent' >>= ('!' \"target\") >>= ('!' \"value\") >>= 'fromJS' :: t'Decoder' 'MisoString'
-- @
--
-- @since 1.15.0.0
currentEvent :: Decoder JSVal
currentEvent = Decoder (pure . Right . envEvent)
-----------------------------------------------------------------------------
-- | The element whose handler is running: the element in your view that
-- has the @on@ attribute, which is also the 'Miso.Types.DOMRef' given to
-- the handler. (miso delegates events, so @event.currentTarget@ is the
-- mount point rather than this element.)
--
-- @
-- -- how far this scrollable element is scrolled
-- 'currentElement' >>= ('!' \"scrollTop\") >>= 'fromJS' :: t'Decoder' Double
-- @
--
-- In a 'Miso.Subscription.Window.windowSub' it is @window@.
--
-- @since 1.15.0.0
currentElement :: Decoder JSVal
currentElement = Decoder (pure . Right . envElement)
-----------------------------------------------------------------------------
-- | Convert a value with its 'FromJSVal' instance.
--
-- @
-- e <- 'currentEvent'
-- x <- 'fromJS' =<< e '!' \"clientX\"   -- :: Double
-- k <- 'fromJS' =<< e '!' \"key\"       -- :: 'MisoString'
-- t <- 'fromJS' =<< e '!' \"target\"    -- :: 'JSVal', the element itself
-- @
--
-- Fails on @null@ and @undefined@; use @'nullable' 'fromJS'@ to accept them.
-- See "Reading values with fromJS" in the module documentation.
--
-- @since 1.15.0.0
fromJS :: FromJSVal a => JSVal -> Decoder a
fromJS v = Decoder $ \_ -> do
  nullish <- isNullish v
  if nullish
    then pure (Left "expected a value, got null or undefined")
    else tryAny (fromJSVal v) >>= \case
      Left e -> pure (Left ("threw " <> ms (show e)))
      Right Nothing -> pure (Left "could not convert this value")
      Right (Just x) -> pure (Right x)
-----------------------------------------------------------------------------
-- | Decode every element of an array, or of anything with a @length@ and
-- numbered elements: @Array@, @FileList@, @TouchList@, @NodeList@,
-- @HTMLCollection@, @DataTransferItemList@, @DOMStringList@, ...
--
-- @
-- -- the names of the files dropped
-- droppedNames :: t'Decoder' ['MisoString']
-- droppedNames = do
--   files <- 'currentEvent' >>= ('!' \"dataTransfer\") >>= ('!' \"files\")
--   'items' (\\f -> 'fromJS' =<< f '!' \"name\") files
-- @
--
-- @since 1.15.0.0
items :: (JSVal -> Decoder a) -> JSVal -> Decoder [a]
items d xs = do
  n <- fromJS =<< xs ! "length"
  traverse (\i -> d =<< xs !! i) [0 .. n - 1 :: Int]
-----------------------------------------------------------------------------
-- | 'Nothing' when the value is @null@ or @undefined@, otherwise decode it.
--
-- @
-- -- event.relatedTarget is null when the pointer came from outside the window
-- 'currentEvent' >>= ('!' \"relatedTarget\") >>= 'nullable' (\\t -> 'fromJS' =<< t '!' \"id\")
--   :: t'Decoder' (Maybe 'MisoString')
-- @
--
-- @since 1.15.0.0
nullable :: (JSVal -> Decoder a) -> JSVal -> Decoder (Maybe a)
nullable d v = Decoder $ \env -> do
  nullish <- isNullish v
  if nullish
    then pure (Right Nothing)
    else fmap Just <$> unDecoder (d v) env
-----------------------------------------------------------------------------
-- | Snapshot a value as a JSON 'Value' and parse it. A string, number or
-- boolean is itself; @null@ and @undefined@ are @null@; an object becomes an
-- object of its own and inherited string, number and boolean properties
-- (objects and functions are left out), and an array-like an array of such
-- snapshots.
--
-- @
-- 'currentEvent' >>= ('!' \"detail\") >>= 'parseWith' 'Miso.JSON.parseJSON' :: t'Decoder' MyDetail
-- @
--
-- Snapshotting an element reads every property it has, including getters
-- such as @offsetWidth@ that force a layout. To read a few properties, use
-- 'parseFields' or '!'.
--
-- @since 1.15.0.0
parseWith :: (Value -> Parser a) -> JSVal -> Decoder a
parseWith parser v = Decoder $ \_ ->
  parseResult parser <$> tryAny (snapshot [] =<< evaluate v)
-----------------------------------------------------------------------------
-- | Parse a value with its 'FromJSON' instance: @'parseWith' 'parseJSON'@.
--
-- Unlike 'fromJS', this checks the JavaScript type on every backend, so it
-- is the way to read a string, number or boolean that might be something
-- else:
--
-- @
-- 'parsed' =<< target '!' \"value\" :: t'Decoder' 'MisoString'   -- fails if value is a number
-- @
--
-- @since 1.15.0.0
parsed :: FromJSON a => JSVal -> Decoder a
parsed = parseWith parseJSON
-----------------------------------------------------------------------------
-- | Like 'parseWith', but the snapshot has only the named properties (those
-- that are strings, numbers or booleans), read in one call. Much cheaper
-- than a full snapshot when an event or element has many properties and
-- you need a few:
--
-- @
-- clientXY :: t'Decoder' (Double, Double)
-- clientXY = 'currentEvent' >>= 'parseFields' [\"clientX\", \"clientY\"] ('withObject' \"event\" $ \\o ->
--   (,) \<$\> o '.:' \"clientX\" \<*\> o '.:' \"clientY\")
-- @
--
-- @since 1.15.0.0
parseFields :: [MisoString] -> (Value -> Parser a) -> JSVal -> Decoder a
parseFields keys parser v = Decoder $ \_ -> do
  result <- tryAny $ do
    keysVal <- toJSVal keys
    v' <- evaluate v
    args <- toJSVal [v', keysVal]
    freeJSVal keysVal
    jsonStep "decoderPick" args
  pure (parseResult parser result)
-----------------------------------------------------------------------------
-- | Snapshot the value at a path below the event (see 'parseWith') and
-- parse it. This is how decoders were written before 1.15:
--
-- @
-- -- event.target.value
-- targetValue :: t'Decoder' 'MisoString'
-- targetValue = 'at' [\"target\"] $ 'withObject' \"target\" $ \\o -> o '.:' \"value\"
-- @
--
-- Unlike '!', a path that runs into @null@ or @undefined@ gives the
-- parser @null@ instead of failing.
at
  :: [MisoString]
  -- ^ Path below the event (e.g. @[\"target\"]@ for @event.target@, @[]@ for the event itself)
  -> (Value -> Parser a)
  -- ^ JSON parser applied to the snapshot at that path
  -> Decoder a
at path parser = Decoder $ \env ->
  parseResult parser <$> tryAny (snapshot path (envEvent env))
-----------------------------------------------------------------------------
-- | Run a JSON parser on a snapshot.
parseResult
  :: (Value -> Parser a)
  -> Either SomeException (Either MisoString Value)
  -> Either MisoString a
parseResult parser = \case
  Left e -> Left ("threw " <> ms (show e))
  Right (Left msg) -> Left msg
  Right (Right v) ->
    case parseEither parser v of
      Left msg -> Left msg
      Right x -> Right x
-----------------------------------------------------------------------------
-- | The JSON snapshot of the value at @path@ below @v@: walk the path a step
-- at a time (a step from @null@ or @undefined@ gives @null@), then
-- @miso.decoderSnapshot@ the value reached.
snapshot :: [MisoString] -> JSVal -> IO (Either MisoString Value)
snapshot path v =
  case path of
    [] -> do
      args <- toJSVal [v]
      jsonStep "decoderSnapshot" args
    k : ks ->
      step v k Nothing >>= \case
        StepOk v' -> snapshot ks v'
        StepNullish -> pure (Right Null)
        StepFailed msg -> pure (Left msg)
-----------------------------------------------------------------------------
-- | Call a JSON-producing helper of the miso bundle (with a JavaScript array
-- of arguments, which this frees) and convert its result to a 'Value'.
--
-- The result's handle is freed unless the 'Value' is a string: on WASM,
-- 'fromJSVal' makes a JavaScript string into a 'MisoString' that /is/ that
-- handle, so freeing it would leave the decoded string dangling.
jsonStep :: MisoString -> JSVal -> IO (Either MisoString Value)
jsonStep helper args =
  step (jsMiso stepFunctions) helper (Just args) >>= \case
    StepOk json -> do
      value <- fromJSVal json
      case value of
        Just (String _) -> pure ()
        _ -> freeJSVal json
      pure (maybe (Left "could not convert this value to JSON") Right value)
    StepNullish -> pure (Left "could not convert this value to JSON")
    StepFailed msg -> pure (Left msg)
-----------------------------------------------------------------------------
-- | A JSON decoder at a t'DecodeTarget'. For 'DecodeTargets', the first
-- path whose snapshot parses wins; if none does, the failure lists why each
-- one failed.
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
    DecodeTargets paths -> Decoder $ \env -> tryPaths env [] paths
  where
    tryPaths env errors = \case
      [] -> pure (Left ("no DecodeTarget matched: " <> joinErrors (reverse errors)))
      path : rest ->
        unDecoder (at path parser) env >>= \case
          Right x -> pure (Right x)
          Left msg -> tryPaths env (("[" <> joinPath path <> "] " <> msg) : errors) rest
    joinErrors = foldr1' (\a b -> a <> "; " <> b)
    joinPath = foldr1' (\a b -> a <> "." <> b)
    foldr1' _ [] = ""
    foldr1' f xs = foldr1 f xs
-----------------------------------------------------------------------------
-- | @o[k]@, converted with 'fromJS'.
prop :: FromJSVal a => JSVal -> MisoString -> Decoder a
prop o k = fromJS =<< o ! k
-----------------------------------------------------------------------------
-- | @o[k]@, converted with 'parsed'.
parsedProp :: FromJSON a => JSVal -> MisoString -> Decoder a
parsedProp o k = parsed =<< o ! k
-----------------------------------------------------------------------------
-- | Empty t'Decoder' for use with events like "click" that do not
-- return any meaningful values
emptyDecoder :: Decoder ()
emptyDecoder = pure ()
-----------------------------------------------------------------------------
-- | Retrieves either "keyCode", "which" or "charCode" field in t'Decoder'
keycodeDecoder :: Decoder KeyCode
keycodeDecoder = KeyCode <$> (keyCodeField =<< currentEvent)
-----------------------------------------------------------------------------
-- | Retrieves either "keyCode", "which" or "charCode" field in t'Decoder',
-- along with shift, ctrl, meta and alt.
keyInfoDecoder :: Decoder KeyInfo
keyInfoDecoder = do
  e <- currentEvent
  KeyInfo
    <$> (KeyCode <$> keyCodeField e)
    <*> parsedProp e "shiftKey"
    <*> parsedProp e "metaKey"
    <*> parsedProp e "ctrlKey"
    <*> parsedProp e "altKey"
-----------------------------------------------------------------------------
-- | @event.keyCode@, else @event.which@, else @event.charCode@.
keyCodeField :: JSVal -> Decoder Int
keyCodeField e = parsedProp e "keyCode" <|> parsedProp e "which" <|> parsedProp e "charCode"
-----------------------------------------------------------------------------
-- | Retrieves "value" field in t'Decoder': @event.target.value@.
--
-- Reads that one property. (Before 1.15 this took a JSON snapshot of every
-- property of @event.target@ on each event, which on an element includes
-- getters such as @offsetWidth@ that force a layout.)
valueDecoder :: Decoder MisoString
valueDecoder = currentEvent >>= (! "target") >>= (`parsedProp` "value")
-----------------------------------------------------------------------------
-- | Retrieves "checked" field in t'Decoder': @event.target.checked@.
checkedDecoder :: Decoder Checked
checkedDecoder = Checked <$> (currentEvent >>= (! "target") >>= (`parsedProp` "checked"))
-----------------------------------------------------------------------------
-- | Pointer t'Decoder' for use with events like "onpointerover"
pointerDecoder :: Decoder PointerEvent
pointerDecoder = currentEvent >>= parseFields pointerFields (withObject "pointerDecoder" $ \o ->
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
    <*> o .: "button")
  where
    pair o x y = liftA2 (,) (o .: x) (o .: y)
-----------------------------------------------------------------------------
-- | The properties 'pointerDecoder' reads.
pointerFields :: [MisoString]
pointerFields =
  [ "pointerType", "pointerId", "isPrimary", "clientX", "clientY"
  , "screenX", "screenY", "offsetX", "offsetY", "pageX", "pageY"
  , "tiltX", "tiltY", "pressure", "button"
  ]
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
clipboardTextDecoder = do
  clipboard <- currentEvent >>= (! "clipboardData")
  fromJS =<< (clipboard # "getData") ("text/plain" :: MisoString)
-----------------------------------------------------------------------------
-- | Everything on the clipboard, for @paste@ (and @copy@ \/ @cut@, where it
-- is what a previous handler put there): every format with its data, and
-- the pasted files. See 'dataTransferDecoder'.
--
-- @since 1.15.0.0
clipboardDecoder :: Decoder DataTransfer
clipboardDecoder = currentEvent >>= (! "clipboardData") >>= dataTransferDecoder
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
dropDecoder = currentEvent >>= (! "dataTransfer") >>= dataTransferDecoder
-----------------------------------------------------------------------------
-- | Decode a
-- <https://developer.mozilla.org/en-US/docs/Web/API/DataTransfer DataTransfer>:
-- its @types@, @getData(type)@ for each type other than @\"Files\"@, its
-- @files@ (see 'fileInfoDecoder'), @dropEffect@ and @effectAllowed@.
--
-- @since 1.15.0.0
dataTransferDecoder :: JSVal -> Decoder DataTransfer
dataTransferDecoder dt = do
  types <- items fromJS =<< dt ! "types"
  datas <- traverse dataOf (filter (/= "Files") types)
  files <- (items fileInfoDecoder =<< dt ! "files") <|> pure []
  dropEffect <- prop dt "dropEffect" <|> pure "none"
  effectAllowed <- prop dt "effectAllowed" <|> pure "uninitialized"
  pure (DataTransfer types datas files dropEffect effectAllowed)
  where
    dataOf format = (,) format <$> (fromJS =<< (dt # "getData") format)
-----------------------------------------------------------------------------
-- | The files chosen in an @\<input type=file\>@, for @change@:
-- @event.target.files@.
--
-- @
-- 'Miso.Html.Element.input_' [ 'Miso.Html.Property.type_' \"file\", 'Miso.Html.Property.multiple_' True, 'Miso.Event.on' \"change\" 'filesDecoder' (\\fs _ _ -> Chose fs) ]
-- @
--
-- To read a file's contents, decode the @File@ objects themselves
-- (@'currentEvent' >>= ('!' \"target\") >>= ('!' \"files\") >>= 'items' 'fromJS' :: t'Decoder' ['JSVal']@)
-- and read them in @update@.
--
-- @since 1.15.0.0
filesDecoder :: Decoder [FileInfo]
filesDecoder = currentEvent >>= (! "target") >>= (! "files") >>= items fileInfoDecoder
-----------------------------------------------------------------------------
-- | Decode a
-- <https://developer.mozilla.org/en-US/docs/Web/API/File File>: its name,
-- type, size and modification time.
--
-- @since 1.15.0.0
fileInfoDecoder :: JSVal -> Decoder FileInfo
fileInfoDecoder f =
  FileInfo
    <$> prop f "name"
    <*> prop f "type"
    <*> prop f "size"
    <*> prop f "lastModified"
-----------------------------------------------------------------------------
-- | Decode a
-- <https://developer.mozilla.org/en-US/docs/Web/API/DOMRect DOMRect>, e.g.
-- the result of @getBoundingClientRect()@ or an item of @getClientRects()@.
--
-- @since 1.15.0.0
domRectDecoder :: JSVal -> Decoder DOMRect
domRectDecoder r =
  DOMRect
    <$> prop r "x"
    <*> prop r "y"
    <*> prop r "width"
    <*> prop r "height"
    <*> prop r "top"
    <*> prop r "right"
    <*> prop r "bottom"
    <*> prop r "left"
-----------------------------------------------------------------------------
-- | The handler's element's @getBoundingClientRect()@, measured as the
-- event happens (see 'currentElement').
--
-- @
-- -- where on the element the click landed, from 0 to 1
-- clickFraction :: t'Decoder' Double
-- clickFraction = do
--   rect <- 'boundingClientRectDecoder'
--   x <- 'currentEvent' >>= ('!' \"clientX\") >>= 'fromJS'
--   pure ((x - 'rectLeft' rect) \/ 'rectWidth' rect)
-- @
--
-- @since 1.15.0.0
boundingClientRectDecoder :: Decoder DOMRect
boundingClientRectDecoder = do
  el <- currentElement
  domRectDecoder =<< (el # "getBoundingClientRect") ()
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
modifierStateDecoder key = do
  e <- currentEvent
  fromJS =<< (e # "getModifierState") key
-----------------------------------------------------------------------------
-- | The text the user has selected in the page, as the event happens:
-- @window.getSelection().toString()@. Useful on @mouseup@ and @keyup@.
-- (For the selection inside an @\<input\>@ or @\<textarea\>@, use
-- 'inputSelectionDecoder'.)
--
-- @since 1.15.0.0
selectionTextDecoder :: Decoder MisoString
selectionTextDecoder = do
  selection <- (global # "getSelection") ()
  fromJS =<< (selection # "toString") ()
-----------------------------------------------------------------------------
-- | @(selectionStart, selectionEnd)@ of the @\<input\>@ or @\<textarea\>@
-- the event happened in: the caret position when they are equal. For
-- @select@, @keyup@, @click@ and @input@. Fails for inputs that have no
-- text selection (e.g. @type=number@).
--
-- @since 1.15.0.0
inputSelectionDecoder :: Decoder (Int, Int)
inputSelectionDecoder = do
  target <- currentEvent >>= (! "target")
  (,) <$> prop target "selectionStart" <*> prop target "selectionEnd"
-----------------------------------------------------------------------------
-- | The values of the selected options of a @\<select multiple\>@, for
-- @change@: @event.target.selectedOptions@. ('valueDecoder' gives only the
-- first one.)
--
-- @since 1.15.0.0
selectedOptionsDecoder :: Decoder [MisoString]
selectedOptionsDecoder =
  currentEvent >>= (! "target") >>= (! "selectedOptions") >>= items (`prop` "value")
-----------------------------------------------------------------------------
