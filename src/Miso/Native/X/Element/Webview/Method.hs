-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Webview.Method
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- __N.B.__ The Desktop-only @cookies.*@ methods
-- (@cookies.get@ / @cookies.set@ / @cookies.remove@ / @cookies.flushStore@)
-- are not modelled here. They can be invoked directly with
-- 'Miso.Native.FFI.invokeExec' if required.
--
----------------------------------------------------------------------------
module Miso.Native.X.Element.Webview.Method
  ( -- *** Methods
    reload
  , eval
  ) where
-----------------------------------------------------------------------------
import           Miso hiding (eval, reload)
import           Miso.Native.FFI (invokeExec)
-----------------------------------------------------------------------------
-- | Params object for 'eval'.
newtype Eval = Eval MisoString
-----------------------------------------------------------------------------
instance ToJSVal Eval where
  toJSVal (Eval func) = do
    o <- create
    set "func" func o
    toJSVal o
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#reload
--
-- Reloads the selected \<webview\>.
--
-- > reload "#myWebview" Reloaded ReloadFailed
--
reload
  :: MisoString
  -> action
  -> (MisoString -> action)
  -> Effect context props model action
reload selector action = invokeExec "reload" selector () (\() -> action)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#eval
--
-- Calls a JavaScript function inside the selected \<webview\>.
--
-- > eval "#myWebview" "alert('hi')" Evaluated EvalFailed
--
eval
  :: MisoString
  -> MisoString
  -- ^ JavaScript function to evaluate
  -> action
  -> (MisoString -> action)
  -> Effect context props model action
eval selector func action =
  invokeExec "eval" selector (Eval func) (\() -> action)
-----------------------------------------------------------------------------
