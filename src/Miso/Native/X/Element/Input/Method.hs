-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Input.Method
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.Input.Method
  ( -- *** Methods
    focus
  , blur
  , setValue
  , setSelectionRange
  , getValue
    -- *** Types
  , InputValue (..)
  ) where
-----------------------------------------------------------------------------
import Miso hiding (focus, blur, setValue, setSelectionRange)
import Miso.Native.FFI (invokeExec)
-----------------------------------------------------------------------------
-- | Result of calling 'getValue'.
data InputValue
  = InputValue
  { value :: MisoString
    -- ^ The current input content
  , selectionStart :: Int
    -- ^ Start position of the selection
  , selectionEnd :: Int
    -- ^ End position of the selection
  , isComposing :: Bool
    -- ^ Whether the input is mid-composition (IME)
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSVal InputValue where
  fromJSVal o = do
    let readProp name = fromJSValUnchecked =<< o ! (name :: MisoString)
    value          <- readProp "value"
    selectionStart <- readProp "selectionStart"
    selectionEnd   <- readProp "selectionEnd"
    isComposing    <- readProp "isComposing"
    pure $ Just InputValue {..}
-----------------------------------------------------------------------------
-- | Params object for 'setValue'.
newtype SetValue = SetValue MisoString
-----------------------------------------------------------------------------
instance ToJSVal SetValue where
  toJSVal (SetValue v) = do
    o <- create
    set "value" v o
    toJSVal o
-----------------------------------------------------------------------------
-- | Params object for 'setSelectionRange'.
data SetSelectionRange = SetSelectionRange Int Int
-----------------------------------------------------------------------------
instance ToJSVal SetSelectionRange where
  toJSVal (SetSelectionRange s e) = do
    o <- create
    set "selectionStart" s o
    set "selectionEnd" e o
    toJSVal o
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#focus
--
-- Requests focus for the selected \<input\>.
--
-- > focus "#myInput" Focused FocusFailed
--
focus
  :: MisoString
  -> action
  -> (MisoString -> action)
  -> Effect context props model action
focus selector action = invokeExec "focus" selector () (\() -> action)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#blur
--
-- Releases focus for the selected \<input\>.
--
-- > blur "#myInput" Blurred BlurFailed
--
blur
  :: MisoString
  -> action
  -> (MisoString -> action)
  -> Effect context props model action
blur selector action = invokeExec "blur" selector () (\() -> action)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#setvalue
--
-- Sets the input content of the selected \<input\>.
--
-- > setValue "#myInput" "hello" ValueSet ValueSetFailed
--
setValue
  :: MisoString
  -> MisoString
  -> action
  -> (MisoString -> action)
  -> Effect context props model action
setValue selector v action =
  invokeExec "setValue" selector (SetValue v) (\() -> action)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#setselectionrange
--
-- Sets the selection range of the selected \<input\>.
--
-- > setSelectionRange "#myInput" 0 3 RangeSet RangeSetFailed
--
setSelectionRange
  :: MisoString
  -> Int
  -> Int
  -> action
  -> (MisoString -> action)
  -> Effect context props model action
setSelectionRange selector s e action =
  invokeExec "setSelectionRange" selector (SetSelectionRange s e) (\() -> action)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#getvalue
--
-- Retrieves the current value (and selection) of the selected \<input\>.
--
-- > getValue "#myInput" GotValue GetValueFailed
--
getValue
  :: MisoString
  -> (InputValue -> action)
  -> (MisoString -> action)
  -> Effect context props model action
getValue selector = invokeExec "getValue" selector ()
-----------------------------------------------------------------------------
