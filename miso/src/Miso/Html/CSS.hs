{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.CSS
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Html.CSS where

import qualified Data.Map        as M
import           Data.Monoid     ( (<>) )

import           Miso.Html.Types ( Attribute ( C )  )
import           Miso.Html.Internal ( MisoString )

-- | Constructs `CSS` for a DOM Element
--
-- > import qualified Data.Map as M
-- > div_ [ style_  $ M.singleton "background" "red" ] [ ]
--
-- <https://developer.mozilla.org/en-US/docs/Web/CSS>
--
style_ :: M.Map MisoString MisoString -> Attribute action
style_ = C "style" . M.foldrWithKey go mempty
  where
    go :: MisoString -> MisoString -> MisoString -> MisoString
    go k v xs = mconcat [ k, ":", v, ";" ] <> xs


-- | Constructs raw `CSS` for a DOM Element
--
-- > div_ [ styleRaw_ "background:red;" ] [ ]
--
styleRaw_ :: MisoString -> Attribute action
styleRaw_ = C "style"


