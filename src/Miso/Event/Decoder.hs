-----------------------------------------------------------------------------
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE MultilineStrings           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Event.Decoder
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Event.Decoder
  ( -- ** Types
    Decoder (..)
  , DecodeTarget (..)
  , Parser (..)
  , DecodeFailure (..)
    -- ** Parser
  , parseEither
    -- ** Combinators
  , at
    -- ** Decoders
  , emptyDecoder
  , keycodeDecoder
  , keyInfoDecoder
  , checkedDecoder
  , valueDecoder
  , pointerDecoder
    -- ** Utils
  , withObject
  , (.:)
    -- ** Internal
  , executeDecoder
  ) where
-----------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Except
import           Control.Applicative
import           Language.Javascript.JSaddle
-----------------------------------------------------------------------------
import qualified Miso.String as MS
import           Miso.Event.Types
import           Miso.String
import           Miso.FFI (Event (..))
import qualified Miso.FFI.Internal as FFI
-----------------------------------------------------------------------------
-- | Data type representing path (consisting of field names) within event object
-- where a decoder should be applied.
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
-- | t'Decoder' data type for parsing events
data Decoder a
  = Decoder
  { decoder :: Event -> Parser a
    -- ^ FromJSON-based Event decoder
  , decodeAt :: DecodeTarget
    -- ^ Location in DOM of where to decode
  }
-----------------------------------------------------------------------------
newtype Parser a = Parser { runParser :: ExceptT [DecodeFailure] JSM a }
  deriving (Applicative, Monad, MonadError [DecodeFailure], Functor, Alternative, MonadJSM, MonadIO)
-----------------------------------------------------------------------------
parseEither :: Event -> Decoder a -> JSM (Either [DecodeFailure] a)
parseEither event Decoder {..} | Parser m <- decoder event = runExceptT m
-----------------------------------------------------------------------------
data DecodeFailure
  = PropertyNotFound MisoString
  | DecodeFailure MisoString
-----------------------------------------------------------------------------
-- | Smart constructor for building a t'Decoder'.
at :: [MisoString] -> (Event -> Parser a) -> Decoder a
at decodeAt decoder = Decoder { decodeAt = DecodeTarget decodeAt, .. }
-----------------------------------------------------------------------------
withObject :: (Event -> Parser a) -> Event -> Parser a
withObject f x = f x
-----------------------------------------------------------------------------
(.:) :: FromJSVal a => Event -> MisoString -> Parser a
(.:) (Event event) key = do
  result <- liftJSM $ do
    result <- unsafeGetProp key (Object event)
    maybeNullOrUndefined result
  case result of
    Nothing -> do
      throwError [PropertyNotFound key]
    Just jsvalue ->
      liftJSM (fromJSVal jsvalue) >>= \case
        Nothing ->
          throwError [DecodeFailure key]
        Just x ->
          pure x
-----------------------------------------------------------------------------
-- | Empty t'Decoder' for use with events like "click" that do not
-- return any meaningful values
emptyDecoder :: Decoder ()
emptyDecoder = mempty `at` go
  where
    go = withObject $ \_ -> pure ()
-----------------------------------------------------------------------------
-- | Retrieves either "keyCode", "which" or "charCode" field in t'Decoder'
keycodeDecoder :: Decoder KeyCode
keycodeDecoder = Decoder {..}
  where
    decodeAt = DecodeTarget mempty
    decoder = withObject $ \o ->
       KeyCode <$> (o .: "keyCode" <|> o .: "which" <|> o .: "charCode")
-----------------------------------------------------------------------------
-- | Retrieves either "keyCode", "which" or "charCode" field in t'Decoder',
-- along with shift, ctrl, meta and alt.
keyInfoDecoder :: Decoder KeyInfo
keyInfoDecoder = Decoder {..}
  where
    decodeAt =
      DecodeTarget mempty
    decoder =
      withObject $ \o ->
        KeyInfo
          <$> (o .: "keyCode" <|> o .: "which" <|> o .: "charCode")
          <*> o .: "shiftKey"
          <*> o .: "metaKey"
          <*> o .: "ctrlKey"
          <*> o .: "altKey"
-----------------------------------------------------------------------------
-- | Retrieves "value" field in t'Decoder'
valueDecoder :: Decoder MisoString
valueDecoder = Decoder {..}
  where
    decodeAt = DecodeTarget ["target"]
    decoder = withObject $ \o -> o .: "value"
-----------------------------------------------------------------------------
-- | Retrieves "checked" field in t'Decoder'
checkedDecoder :: Decoder Checked
checkedDecoder = Decoder {..}
  where
    decodeAt = DecodeTarget ["target"]
    decoder = withObject $ \o ->
      Checked <$> (o .: "checked")
-----------------------------------------------------------------------------
-- | Pointer t'Decoder' for use with events like "onpointerover"
pointerDecoder :: Decoder PointerEvent
pointerDecoder = Decoder {..}
  where
    pair o x y = liftA2 (,) (o .: x) (o .: y)
    decodeAt = DecodeTarget mempty
    decoder = withObject $ \o ->
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
-----------------------------------------------------------------------------
-- | Helper function used internall to execute a t'Decoder'
executeDecoder
  :: Decoder a
  -> MisoString
  -> JSVal
  -> (a -> JSM ())
  -> JSM ()
executeDecoder decode@Decoder {..} eventName e callback = do
  decodeAtVal <- toJSVal decodeAt
  syntheticEvent <- fromJSVal =<< FFI.eventJSON decodeAtVal e
  case (syntheticEvent :: Maybe FFI.Event) of
    Nothing ->
      FFI.consoleError """
       Internal error: Could not create synthetic event. Please
       check your 'Decoder' and the `decodeAtVal` field.
      """
    Just (event :: FFI.Event) ->
      parseEither event decode >>= \case
        Left errors ->
          forM_ errors $ \case
            PropertyNotFound key -> do
              FFI.consoleError $ MS.intercalate " "
                [ "Property"
                , "\"" <> key <> "\" "
                , "not found on event: "
                , eventName
                ]
            DecodeFailure key -> do
              FFI.consoleError $ MS.intercalate " "
                [ "Property"
                , "\"" <> key <> "\" "
                , "failed to decode on event: "
                , eventName
                ]
        Right r ->
          callback r
-----------------------------------------------------------------------------
