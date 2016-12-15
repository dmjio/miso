{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE StandaloneDeriving    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Example usage:
--
-- @
-- import Miso.Html
-- import qualified Data.Text as T
--
-- data IntAction = Add | Subtract
--
-- intView :: Int -> View IntAction
-- intView n = div_ [ class_ "main" ] [
--    btn_ [ onClick Add ] [ text_ "+" ]
--  , text_ $ T.pack (show n)
--  , btn_ [ onClick Subtract ] [ text_ "-" ]
--  ]
-- @
--
-- More information on how to use `miso` and `miso-html` is available on the miso wiki:
--
-- <http://github.com/haskell-miso/miso/wiki>
--
----------------------------------------------------------------------------
module Miso.Html
   (  module Miso.Html.Element
    , module Miso.Html.Event
    , module Miso.Html.Internal
    , module Miso.Html.Property
    , module Miso.Types
   ) where

import Miso.Html.Element hiding (form_)
import Miso.Html.Event
import Miso.Html.Internal
import Miso.Html.Property
import Miso.Types
