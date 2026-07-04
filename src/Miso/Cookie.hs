-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Cookie
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Cookie" wraps the browser's
-- <https://developer.mozilla.org/en-US/docs/Web/API/CookieStore CookieStore API>
-- as 'Miso.Effect.Effect' combinators that integrate directly into the
-- Model-View-Update loop.
--
-- = Quick start
--
-- @
-- import "Miso"
-- import "Miso.Cookie"
--
-- data Action
--   = LoadSession
--   | GotSession (Maybe 'Cookie')
--   | SessionError 'Miso.String.MisoString'
--   | SaveTheme
--   | ThemeSaved
--
-- update :: Action -> 'Miso.Effect.Effect' p props Model Action
-- update LoadSession =
--   'cookieGet' \"session\" GotSession SessionError
-- update SaveTheme =
--   'cookieSet' \"theme\" \"dark\" ThemeSaved SessionError
-- update _ = pure ()
-- @
--
-- = Types
--
-- * 'Cookie' — a single cookie with all standard fields
-- * 'CookieChangeEvent' — payload for 'Miso.Subscription.Cookie.cookieChangeSub'
--
-- = API groups
--
-- * __Read__: 'cookieGet', 'cookieGetAll'
-- * __Write__: 'cookieSet'
-- * __Delete__: 'cookieDelete'
--
-- = Availability
--
-- The CookieStore API requires a
-- <https://developer.mozilla.org/en-US/docs/Web/Security/Secure_Contexts secure context>
-- (HTTPS or @localhost@) and is not yet universally supported. Operations
-- silently call the error callback when the API is unavailable.
--
-- = See also
--
-- * "Miso.Subscription.Cookie" — 'Miso.Subscription.Cookie.cookieChangeSub'
--   for subscribing to cookie change events
-- * "Miso.FFI" — raw FFI primitives ('Miso.FFI.cookieGet' etc.) for
--   advanced use-cases
-----------------------------------------------------------------------------
module Miso.Cookie
  ( -- ** Types
    Cookie (..)
  , CookieChangeEvent (..)
    -- ** Read
  , cookieGet
  , cookieGetAll
    -- ** Write
  , cookieSet
    -- ** Delete
  , cookieDelete
    -- ** Construction
  , defaultCookie
  ) where
-----------------------------------------------------------------------------
import           Control.Monad ((<=<), forM_, join)
import           Prelude hiding ((!!))
-----------------------------------------------------------------------------
import           Miso.DSL
import           Miso.Effect
import           Miso.String (MisoString)
import qualified Miso.FFI.Internal as FFI
-----------------------------------------------------------------------------
-- | A cookie from the
-- <https://developer.mozilla.org/en-US/docs/Web/API/CookieStore CookieStore API>.
--
-- Fields map directly to the browser's
-- <https://developer.mozilla.org/en-US/docs/Web/API/CookieStore/get#return_value CookieListItem>.
data Cookie = Cookie
  { cookieName        :: MisoString
  -- ^ Cookie name (empty string for nameless cookies)
  , cookieValue       :: Maybe MisoString
  -- ^ Cookie value
  , cookieDomain      :: Maybe MisoString
  -- ^ Cookie domain (@Nothing@ when unset — host-only cookie)
  , cookiePath        :: MisoString
  -- ^ Cookie path
  , cookieExpires     :: Maybe Double
  -- ^ Expiry as Unix milliseconds (@Nothing@ for session cookies)
  , cookieSecure      :: Bool
  -- ^ @Secure@ flag
  , cookieSameSite    :: MisoString
  -- ^ @SameSite@ value: @\"strict\"@, @\"lax\"@, or @\"none\"@
  , cookiePartitioned :: Bool
  -- ^ @Partitioned@ flag (CHIPS — Cookies Having Independent Partitioned State)
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal Cookie where
  toJSVal Cookie {..} = do
    o <- create
    FFI.set "name"        cookieName        o
    FFI.set "value"       cookieValue       o
    FFI.set "path"        cookiePath        o
    FFI.set "secure"      cookieSecure      o
    FFI.set "sameSite"    cookieSameSite    o
    FFI.set "partitioned" cookiePartitioned o
    forM_ cookieDomain  $ \d -> FFI.set "domain"  d o
    forM_ cookieExpires $ \e -> FFI.set "expires" e o
    toJSVal o
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance FromJSVal Cookie where
  fromJSVal v = do
    name_        <- fromJSVal =<< v ! "name"
    value_       <- fromJSVal =<< v ! "value"
    domain_      <- fromJSVal =<< v ! "domain"
    path_        <- fromJSVal =<< v ! "path"
    expires_     <- fromJSVal =<< v ! "expires"
    secure_      <- fromJSVal =<< v ! "secure"
    sameSite_    <- fromJSVal =<< v ! "sameSite"
    partitioned_ <- fromJSVal =<< v ! "partitioned"
    pure $ do
      n   <- name_
      vl  <- value_
      p   <- path_
      sec <- secure_
      ss  <- sameSite_
      par <- partitioned_
      pure Cookie
        { cookieName        = n
        , cookieValue       = vl
        , cookieDomain      = join domain_
        , cookiePath        = p
        , cookieExpires     = join expires_
        , cookieSecure      = sec
        , cookieSameSite    = ss
        , cookiePartitioned = par
        }
  {-# INLINE fromJSVal #-}
-----------------------------------------------------------------------------
-- | The event payload delivered to
-- <https://developer.mozilla.org/en-US/docs/Web/API/CookieStore/change_event cookieStore change>
-- listeners. Consumed by 'Miso.Subscription.Cookie.cookieChangeSub'.
data CookieChangeEvent = CookieChangeEvent
  { cookiesChanged :: [Cookie]
  -- ^ Cookies that were added or updated
  , cookiesDeleted :: [Cookie]
  -- ^ Cookies that were deleted
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSVal CookieChangeEvent where
  fromJSVal ev = do
    changed_ <- fromJSValUnchecked =<< ev ! "changed"
    deleted_ <- fromJSValUnchecked =<< ev ! "deleted"
    pure (CookieChangeEvent <$> changed_ <*> deleted_)
  {-# INLINE fromJSVal #-}
-----------------------------------------------------------------------------
instance ToJSVal CookieChangeEvent where
  toJSVal CookieChangeEvent {..} = do
    o <- create
    FFI.set "changed" cookiesChanged o
    FFI.set "deleted" cookiesDeleted o
    toJSVal o
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance ToArgs CookieChangeEvent where
  toArgs ev = (:[]) <$> toJSVal ev
  {-# INLINE toArgs #-}
-----------------------------------------------------------------------------
-- | Retrieve a cookie value by name.
--
-- Calls @successful@ with @'Just' value@ when found, @'Nothing'@ when
-- absent, or @errorful@ with the error message if the operation fails.
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CookieStore/get>
cookieGet
  :: MisoString
  -- ^ Cookie name
  -> (Maybe MisoString -> action)
  -- ^ Successful callback (@Nothing@ when the cookie is absent)
  -> (MisoString -> action)
  -- ^ Errorful callback
  -> Effect parent props model action
cookieGet name successful errorful = withSink $ \sink ->
  FFI.cookieGet name
    (sink . successful <=< fromJSVal)
    (sink . errorful)
-----------------------------------------------------------------------------
-- | Retrieve all cookies visible to the current document.
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CookieStore/getAll>
cookieGetAll
  :: ([Cookie] -> action)
  -- ^ Successful callback
  -> (MisoString -> action)
  -- ^ Errorful callback
  -> Effect parent props model action
cookieGetAll successful errorful = withSink $ \sink -> do
  FFI.cookieGetAll
    (sink . successful <=< fromJSValUnchecked)
    (sink . errorful)
-----------------------------------------------------------------------------
-- | A 'Cookie' with sensible defaults: @path = "/"@, session expiry,
-- @SameSite = "lax"@, not secure, not partitioned, no domain restriction.
defaultCookie :: MisoString -> MisoString -> Cookie
defaultCookie name value = Cookie
  { cookieName        = name
  , cookieValue       = Just value
  , cookieDomain      = Nothing
  , cookiePath        = "/"
  , cookieExpires     = Nothing
  , cookieSecure      = False
  , cookieSameSite    = "lax"
  , cookiePartitioned = False
  }
-----------------------------------------------------------------------------
-- | Set a cookie via the
-- <https://developer.mozilla.org/en-US/docs/Web/API/CookieStore CookieStore API>.
--
-- Use 'defaultCookie' to construct a 'Cookie' with sensible defaults, or
-- supply a fully specified 'Cookie' record for custom path, domain, expiry etc.
--
-- __Important:__ You cannot use both @expires@ and @maxAge@ in the same call.
-- If you do, the @set()@ method will fail with a @TypeError@.
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CookieStore/set>
cookieSet
  :: Cookie
  -- ^ Cookie to set (use 'defaultCookie' for the common case)
  -> action
  -- ^ Successful callback
  -> (MisoString -> action)
  -- ^ Errorful callback
  -> Effect parent props model action
cookieSet cookie successful errorful = withSink $ \sink -> do
  c_ <- toJSVal cookie
  FFI.cookieSet c_ (sink successful) (sink . errorful)
-----------------------------------------------------------------------------
-- | Delete a cookie by name.
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CookieStore/delete>
cookieDelete
  :: MisoString
  -- ^ Cookie name
  -> action
  -- ^ Successful callback
  -> (MisoString -> action)
  -- ^ Errorful callback
  -> Effect parent props model action
cookieDelete name successful errorful = withSink $ \sink ->
  FFI.cookieDelete name (sink successful) (sink . errorful)
-----------------------------------------------------------------------------
