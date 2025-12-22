-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.CSS.Color
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Miso.CSS.Color
  ( -- *** Types
    Color (RGB, RGBA, HSL, HSLA, Hex)
    -- *** Smart constructor
  , rgba
  , rgb
  , hsl
  , hsla
  , hex
  , var
    -- *** Render
  , renderColor
    -- *** Colors
  , transparent
  , aliceblue
  , antiquewhite
  , aqua
  , aquamarine
  , azure
  , beige
  , bisque
  , black
  , blanchedalmond
  , blue
  , blueviolet
  , brown
  , burlywood
  , cadetblue
  , chartreuse
  , chocolate
  , coral
  , cornflowerblue
  , cornsilk
  , crimson
  , cyan
  , darkblue
  , darkcyan
  , darkgoldenrod
  , darkgray
  , darkgreen
  , darkgrey
  , darkkhaki
  , darkmagenta
  , darkolivegreen
  , darkorange
  , darkorchid
  , darkred
  , darksalmon
  , darkseagreen
  , darkslateblue
  , darkslategray
  , darkslategrey
  , darkturquoise
  , darkviolet
  , deeppink
  , deepskyblue
  , dimgray
  , dimgrey
  , dodgerblue
  , firebrick
  , floralwhite
  , forestgreen
  , fuchsia
  , gainsboro
  , ghostwhite
  , gold
  , goldenrod
  , gray
  , green
  , greenyellow
  , grey
  , honeydew
  , hotpink
  , indianred
  , indigo
  , ivory
  , khaki
  , lavender
  , lavenderblush
  , lawngreen
  , lemonchiffon
  , lightblue
  , lightcoral
  , lightcyan
  , lightgoldenrodyellow
  , lightgray
  , lightgreen
  , lightgrey
  , lightpink
  , lightsalmon
  , lightseagreen
  , lightskyblue
  , lightslategray
  , lightslategrey
  , lightsteelblue
  , lightyellow
  , lime
  , limegreen
  , linen
  , magenta
  , maroon
  , mediumaquamarine
  , mediumblue
  , mediumorchid
  , mediumpurple
  , mediumseagreen
  , mediumslateblue
  , mediumspringgreen
  , mediumturquoise
  , mediumvioletred
  , midnightblue
  , mintcream
  , mistyrose
  , moccasin
  , navajowhite
  , navy
  , oldlace
  , olive
  , olivedrab
  , orange
  , orangered
  , orchid
  , palegoldenrod
  , palegreen
  , paleturquoise
  , palevioletred
  , papayawhip
  , peachpuff
  , peru
  , pink
  , plum
  , powderblue
  , purple
  , red
  , rosybrown
  , royalblue
  , saddlebrown
  , salmon
  , sandybrown
  , seagreen
  , seashell
  , sienna
  , silver
  , skyblue
  , slateblue
  , slategray
  , slategrey
  , snow
  , springgreen
  , steelblue
  , tan
  , teal
  , thistle
  , tomato
  , turquoise
  , violet
  , wheat
  , white
  , whitesmoke
  , yellow
  , yellowgreen
  ) where
-----------------------------------------------------------------------------
import           Miso.String (MisoString, ms)
import qualified Miso.String as MS
-----------------------------------------------------------------------------
import           Data.Proxy
import           GHC.TypeLits
import           GHC.OverloadedLabels
import           Miso.DSL (ToJSVal(..), ToArgs(..))
import           Prelude hiding (tan)
-----------------------------------------------------------------------------
-- | Data type for expressing Color
data Color
  = RGBA Int Int Int Double
  -- ^ Red, green, blue and alpha transparency. See [here](https://www.w3schools.com/colors/colors_rgb.asp)
  | RGB Int Int Int
  -- ^ Red, green, blue. See [here](https://www.w3schools.com/colors/colors_rgb.asp)
  | HSL Int Int Int
  -- ^ Hue, saturation, light. See [here](https://www.w3schools.com/colors/colors_hsl.asp)
  | HSLA Int Int Int Double
  -- ^ Hue, saturation, light and alpha transparency. See [here](https://www.w3schools.com/colors/colors_hsl.asp)
  | Hex MisoString
  -- ^ Hexadecimal representation of a color. See [here](https://www.w3schools.com/colors/colors_hexadecimal.asp)
  | VarColor MisoString
  -- ^ A CSS variable
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | 'IsLabel' instance on 'Color'
--
-- @
-- grey :: Color
-- grey = #cccccc
-- @
instance KnownSymbol color => IsLabel color Color where
  fromLabel = Hex (ms color)
    where
      color = symbolVal (Proxy @color)
-----------------------------------------------------------------------------
-- | 'IsLabel' instance on 'MisoString' for construction of hex colors as strings
--
-- @
-- grey :: MisoString
-- grey = #cccccc
-- @
instance KnownSymbol hex => IsLabel hex MisoString where
  fromLabel = ms ("#" <> symbolVal (Proxy @hex))
-----------------------------------------------------------------------------
-- | 'ToArgs' instance for 'Color'
instance ToArgs Color where
  makeArgs color = (:[]) <$> toJSVal color
-----------------------------------------------------------------------------
-- | 'ToJSVal' instance for 'Color'
instance ToJSVal Color where
  toJSVal = toJSVal . renderColor
-----------------------------------------------------------------------------
-- | Renders a 'Color' as 'MisoString'
--
-- >>> renderColor (hex "ccc")
-- "#ccc"
--
renderColor :: Color -> MisoString
renderColor (RGBA r g b a) = "rgba(" <> values <> ")"
  where
    values = MS.intercalate ","
      [ MS.ms r
      , MS.ms g
      , MS.ms b
      , MS.ms a
      ]
renderColor (RGB r g b) = "rgb(" <> values <> ")"
  where
    values = MS.intercalate ","
      [ MS.ms r
      , MS.ms g
      , MS.ms b
      ]
renderColor (HSLA h s l a) = "hsla(" <> values <> ")"
  where
    values = MS.intercalate ","
      [ MS.ms h
      , MS.ms s
      , MS.ms l
      , MS.ms a
      ]
renderColor (HSL h s l) = "hsl(" <> values <> ")"
  where
    values = MS.intercalate ","
      [ MS.ms h
      , MS.ms s
      , MS.ms l
      ]
renderColor (Hex s) = "#" <> s
renderColor (VarColor n) = "var(--" <> n <> ")"
-----------------------------------------------------------------------------
-- | Smart constructor for a [CSS variable](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_cascading_variables/Using_CSS_custom_properties).
--
-- >>> renderColor (var "foo")
-- "var(--foo)"
--
var :: MisoString -> Color
var = VarColor
-----------------------------------------------------------------------------
-- | Smart constructor for an [RGBA](https://www.w3schools.com/css/css_colors_rgb.asp) 'Color' value.
--
-- >>> renderColor (rgba 0 0 0 1.0)
-- "rgba(0,0,0,1.0)"
--
rgba :: Int -> Int -> Int -> Double -> Color
rgba = RGBA
-----------------------------------------------------------------------------
-- | Smart constructor for an [RGB](https://www.w3schools.com/css/css_colors_rgb.asp) 'Color' value.
--
-- >>> renderColor (rgb 0 0 0)
-- "rgb(0,0,0)"
--
rgb :: Int -> Int -> Int -> Color
rgb = RGB
-----------------------------------------------------------------------------
-- | Smart constructor for an [HSL](https://www.w3schools.com/css/css_colors_hsl.asp) 'Color' value.
--
-- >>> renderColor (hsl 0 0 0)
-- "hsl(0,0,0)"
--
hsl :: Int -> Int -> Int -> Color
hsl = HSL
-----------------------------------------------------------------------------
-- | Smart constructor for a [HSLA](https://www.w3schools.com/css/css_colors_hsl.asp) 'Color' value.
--
-- >>> renderColor (hsla 0 0 0 1.0)
-- "hsla(0,0,0,1.0)"
--
hsla :: Int -> Int -> Int -> Double -> Color
hsla = HSLA
-----------------------------------------------------------------------------
-- | Smart constructor for a 'Hex' 'Color'
--
-- >>> renderColor (hsla 0 0 0 1.0)
-- "hsl(0,0,0,1.0)"
--
hex :: MisoString -> Color
hex = Hex
-----------------------------------------------------------------------------
-- | Smart constructor for the 'transparent' color
--
-- >>> renderColor transparent
-- "rgba(0,0,0,0.0)"
--
transparent :: Color
transparent = rgba 0 0 0 0
-- <svg xmlns="http://www.w3.org/2000/svg" width="100" height="100"><rect width="100" height="100" fill="goldenrod"/></svg>
-- data:image/svg+xml;base64,
-----------------------------------------------------------------------------
-- | Smart constructor for the 'aliceblue' 'Color'.
--
-- >>> renderColor aliceblue
-- "rgba(240,248,255,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iYWxpY2VibHVlIi8+PC9zdmc+>>
--
aliceblue :: Color
aliceblue = rgba 240 248 255 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'antiquewhite' 'Color'.
--
-- >>> renderColor antiquewhite
-- "rgba(250,235,215,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iYW50aXF1ZXdoaXRlIi8+PC9zdmc+>>
--
antiquewhite :: Color
antiquewhite = rgba 250 235 215 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'aqua' 'Color'.
--
-- >>> renderColor aqua
-- "rgba(0,255,255,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iYXF1YSIvPjwvc3ZnPg==>>
--
aqua :: Color
aqua = rgba 0 255 255 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'aquamarine' 'Color'.
--
-- >>> renderColor aquamarine
-- "rgba(127,255,212,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iYXF1YW1hcmluZSIvPjwvc3ZnPg==>>
--
aquamarine :: Color
aquamarine = rgba 127 255 212 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'azure' 'Color'.
--
-- >>> renderColor azure
-- "rgba(240,255,255,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iYXp1cmUiLz48L3N2Zz4=>>
--
azure :: Color
azure = rgba 240 255 255 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'beige' 'Color'.
--
-- >>> renderColor beige
-- "rgba(245,245,220,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iYmVpZ2UiLz48L3N2Zz4=>>
--
beige :: Color
beige = rgba 245 245 220 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'bisque' 'Color'.
--
-- >>> renderColor bisque
-- "rgba(255,228,196,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iYmlzcXVlIi8+PC9zdmc+>>
--
bisque :: Color
bisque = rgba 255 228 196 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'black' 'Color'.
--
-- >>> renderColor black
-- "rgba(0,0,0,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iYmxhY2siLz48L3N2Zz4=>>
--
black :: Color
black = rgba 0 0 0 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'blanchedalmond' 'Color'.
--
-- >>> renderColor blanchedalmond
-- "rgba(255,235,205,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iYmxhbmNoZWRhbG1vbmQiLz48L3N2Zz4=>>
--
blanchedalmond :: Color
blanchedalmond = rgba 255 235 205 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'blue' 'Color'.
--
-- >>> renderColor blue
-- "rgba(0,0,255,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iYmx1ZSIvPjwvc3ZnPg==>>
--
blue :: Color
blue = rgba 0 0 255 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'blueviolet' 'Color'.
--
-- >>> renderColor blueviolet
-- "rgba(138,43,226,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iYmx1ZXZpb2xldCIvPjwvc3ZnPg==>>
--
blueviolet :: Color
blueviolet = rgba 138 43 226 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'brown' 'Color'.
--
-- >>> renderColor brown
-- "rgba(165,42,42,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iYnJvd24iLz48L3N2Zz4=>>
--
brown :: Color
brown = rgba 165 42 42 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'burlywood' 'Color'.
--
-- >>> renderColor burlywood
-- "rgba(222,184,135,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iYnVybHl3b29kIi8+PC9zdmc+>>
--
burlywood :: Color
burlywood = rgba 222 184 135 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'cadetblue' 'Color'.
--
-- >>> renderColor cadetblue
-- "rgba(95,158,160,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iY2FkZXRibHVlIi8+PC9zdmc+>>
--
cadetblue :: Color
cadetblue = rgba 95 158 160 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'chartreuse' 'Color'.
--
-- >>> renderColor chartreuse
-- "rgba(127,255,0,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iY2hhcnRyZXVzZSIvPjwvc3ZnPg==>>
--
chartreuse :: Color
chartreuse = rgba 127 255 0 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'chocolate' 'Color'.
--
-- >>> renderColor chocolate
-- "rgba(210,105,30,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iY2hvY29sYXRlIi8+PC9zdmc+>>
--
chocolate :: Color
chocolate = rgba 210 105 30 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'coral' 'Color'.
--
-- >>> renderColor coral
-- "rgba(255,127,80,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iY29yYWwiLz48L3N2Zz4=>>
--
coral :: Color
coral = rgba 255 127 80 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'cornflowerblue' 'Color'.
--
-- >>> renderColor cornflowerblue
-- "rgba(100,149,237,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iY29ybmZsb3dlcmJsdWUiLz48L3N2Zz4=>>
--
cornflowerblue :: Color
cornflowerblue = rgba 100 149 237 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'cornsilk' 'Color'.
--
-- >>> renderColor cornsilk
-- "rgba(255,248,220,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iY29ybnNpbGsiLz48L3N2Zz4=>>
--
cornsilk :: Color
cornsilk = rgba 255 248 220 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'crimson' 'Color'.
--
-- >>> renderColor crimson
-- "rgba(220,20,60,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iY3JpbXNvbiIvPjwvc3ZnPg==>>
--
crimson :: Color
crimson = rgba 220 20 60 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'cyan' 'Color'.
--
-- >>> renderColor cyan
-- "rgba(0,255,255,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iY3lhbiIvPjwvc3ZnPg==>>
--
cyan :: Color
cyan = rgba 0 255 255 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'darkblue' 'Color'.
--
-- >>> renderColor darkblue
-- "rgba(0,0,139,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGFya2JsdWUiLz48L3N2Zz4=>>
--
darkblue :: Color
darkblue = rgba 0 0 139 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'darkcyan' 'Color'.
--
-- >>> renderColor darkcyan
-- "rgba(0,139,139,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGFya2N5YW4iLz48L3N2Zz4=>>
--
darkcyan :: Color
darkcyan = rgba 0 139 139 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'darkgoldenrod' 'Color'.
--
-- >>> renderColor darkgoldenrod
-- "rgba(184,134,11,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGFya2dvbGRlbnJvZCIvPjwvc3ZnPg==>>
--
darkgoldenrod :: Color
darkgoldenrod = rgba 184 134 11 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'darkgray' 'Color'.
--
-- >>> renderColor darkgray
-- "rgba(169,169,169,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGFya2dyYXkiLz48L3N2Zz4=>>
--
darkgray :: Color
darkgray = rgba 169 169 169 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'darkgreen' 'Color'.
--
-- >>> renderColor darkgreen
-- "rgba(0,100,0,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGFya2dyZWVuIi8+PC9zdmc+>>
--
darkgreen :: Color
darkgreen = rgba 0 100 0 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'darkgrey' 'Color'.
--
-- >>> renderColor darkgrey
-- "rgba(169,169,169,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGFya2dyZXkiLz48L3N2Zz4=>>
--
darkgrey :: Color
darkgrey = rgba 169 169 169 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'darkkhaki' 'Color'.
--
-- >>> renderColor darkkhaki
-- "rgba(189,183,107,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGFya2toYWtpIi8+PC9zdmc+>>
--
darkkhaki :: Color
darkkhaki = rgba 189 183 107 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'darkmagenta' 'Color'.
--
-- >>> renderColor darkmagenta
-- "rgba(139,0,139,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGFya21hZ2VudGEiLz48L3N2Zz4=>>
--
darkmagenta :: Color
darkmagenta = rgba 139 0 139 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'darkolivegreen' 'Color'.
--
-- >>> renderColor darkolivegreen
-- "rgba(85,107,47,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGFya29saXZlZ3JlZW4iLz48L3N2Zz4=>>
--
darkolivegreen :: Color
darkolivegreen = rgba 85 107 47 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'darkorange' 'Color'.
--
-- >>> renderColor darkorange
-- "rgba(255,140,0,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGFya29yYW5nZSIvPjwvc3ZnPg==>>
--
darkorange :: Color
darkorange = rgba 255 140 0 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'darkorchid' 'Color'.
--
-- >>> renderColor darkorchid
-- "rgba(153,50,204,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGFya29yY2hpZCIvPjwvc3ZnPg==>>
--
darkorchid :: Color
darkorchid = rgba 153 50 204 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'darkred' 'Color'.
--
-- >>> renderColor darkred
-- "rgba(139,0,0,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGFya3JlZCIvPjwvc3ZnPg==>>
--
darkred :: Color
darkred = rgba 139 0 0 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'darksalmon' 'Color'.
--
-- >>> renderColor darksalmon
-- "rgba(233,150,122,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGFya3NhbG1vbiIvPjwvc3ZnPg==>>
--
darksalmon :: Color
darksalmon = rgba 233 150 122 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'darkseagreen' 'Color'.
--
-- >>> renderColor darkseagreen
-- "rgba(143,188,143,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGFya3NlYWdyZWVuIi8+PC9zdmc+>>
--
darkseagreen :: Color
darkseagreen = rgba 143 188 143 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'darkslateblue' 'Color'.
--
-- >>> renderColor darkslateblue
-- "rgba(72,61,139,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGFya3NsYXRlYmx1ZSIvPjwvc3ZnPg==>>
--
darkslateblue :: Color
darkslateblue = rgba 72 61 139 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'darkslategray' 'Color'.
--
-- >>> renderColor darkslategray
-- "rgba(47,79,79,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGFya3NsYXRlZ3JheSIvPjwvc3ZnPg==>>
--
darkslategray :: Color
darkslategray = rgba 47 79 79 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'darkslategrey' 'Color'.
--
-- >>> renderColor darkslategrey
-- "rgba(47,79,79,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGFya3NsYXRlZ3JleSIvPjwvc3ZnPg==>>
--
darkslategrey :: Color
darkslategrey = rgba 47 79 79 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'darkturquoise' 'Color'.
--
-- >>> renderColor darkturquoise
-- "rgba(0,206,209,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGFya3R1cnF1b2lzZSIvPjwvc3ZnPg==>>
--
darkturquoise :: Color
darkturquoise = rgba 0 206 209 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'darkviolet' 'Color'.
--
-- >>> renderColor darkviolet
-- "rgba(148,0,211,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGFya3Zpb2xldCIvPjwvc3ZnPg==>>
--
darkviolet :: Color
darkviolet = rgba 148 0 211 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'deeppink' 'Color'.
--
-- >>> renderColor deeppink
-- "rgba(255,20,147,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGVlcHBpbmsiLz48L3N2Zz4=>>
--
deeppink :: Color
deeppink = rgba 255 20 147 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'deepskyblue' 'Color'.
--
-- >>> renderColor deepskyblue
-- "rgba(0,191,255,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGVlcHNreWJsdWUiLz48L3N2Zz4=>>
--
deepskyblue :: Color
deepskyblue = rgba 0 191 255 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'dimgray' 'Color'.
--
-- >>> renderColor dimgray
-- "rgba(105,105,105,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGltZ3JheSIvPjwvc3ZnPg==>>
--
dimgray :: Color
dimgray = rgba 105 105 105 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'dimgrey' 'Color'.
--
-- >>> renderColor dimgrey
-- "rgba(105,105,105,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZGltZ3JleSIvPjwvc3ZnPg==>>
--
dimgrey :: Color
dimgrey = rgba 105 105 105 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'dodgerblue' 'Color'.
--
-- >>> renderColor dodgerblue
-- "rgba(30,144,255,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZG9kZ2VyYmx1ZSIvPjwvc3ZnPg==>>
--
dodgerblue :: Color
dodgerblue = rgba 30 144 255 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'firebrick' 'Color'.
--
-- >>> renderColor firebrick
-- "rgba(178,34,34,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZmlyZWJyaWNrIi8+PC9zdmc+>>
--
firebrick :: Color
firebrick = rgba 178 34 34 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'floralwhite' 'Color'.
--
-- >>> renderColor floralwhite
-- "rgba(255,250,240,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZmxvcmFsd2hpdGUiLz48L3N2Zz4=>>
--
floralwhite :: Color
floralwhite = rgba 255 250 240 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'forestgreen' 'Color'.
--
-- >>> renderColor forestgreen
-- "rgba(34,139,34,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZm9yZXN0Z3JlZW4iLz48L3N2Zz4=>>
--
forestgreen :: Color
forestgreen = rgba 34 139 34 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'fuchsia' 'Color'.
--
-- >>> renderColor fuchsia
-- "rgba(255,0,255,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZnVjaHNpYSIvPjwvc3ZnPg==>>
--
fuchsia :: Color
fuchsia = rgba 255 0 255 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'gainsboro' 'Color'.
--
-- >>> renderColor gainsboro
-- "rgba(220,220,220,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZ2FpbnNib3JvIi8+PC9zdmc+>>
--
gainsboro :: Color
gainsboro = rgba 220 220 220 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'ghostwhite' 'Color'.
--
-- >>> renderColor ghostwhite
-- "rgba(248,248,255,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZ2hvc3R3aGl0ZSIvPjwvc3ZnPg==>>
--
ghostwhite :: Color
ghostwhite = rgba 248 248 255 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'gold' 'Color'.
--
-- >>> renderColor gold
-- "rgba(255,215,0,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZ29sZCIvPjwvc3ZnPg==>>
--
gold :: Color
gold = rgba 255 215 0 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'goldenrod' 'Color'.
--
-- >>> renderColor goldenrod
-- "rgba(218,165,32,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZ29sZGVucm9kIi8+PC9zdmc+>>
--
goldenrod :: Color
goldenrod = rgba 218 165 32 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'gray' 'Color'.
--
-- >>> renderColor gray
-- "rgba(128,128,128,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZ3JheSIvPjwvc3ZnPg==>>
--
gray :: Color
gray = rgba 128 128 128 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'green' 'Color'.
--
-- >>> renderColor green
-- "rgba(0,128,0,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZ3JlZW4iLz48L3N2Zz4=>>
--
green :: Color
green = rgba 0 128 0 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'greenyellow' 'Color'.
--
-- >>> renderColor greenyellow
-- "rgba(173,255,47,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZ3JlZW55ZWxsb3ciLz48L3N2Zz4=>>
--
greenyellow :: Color
greenyellow = rgba 173 255 47 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'grey' 'Color'.
--
-- >>> renderColor grey
-- "rgba(128,128,128,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iZ3JleSIvPjwvc3ZnPg==>>
--
grey :: Color
grey = rgba 128 128 128 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'honeydew' 'Color'.
--
-- >>> renderColor honeydew
-- "rgba(240,255,240,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iaG9uZXlkZXciLz48L3N2Zz4=>>
--
honeydew :: Color
honeydew = rgba 240 255 240 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'hotpink' 'Color'.
--
-- >>> renderColor hotpink
-- "rgba(255,105,180,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iaG90cGluayIvPjwvc3ZnPg==>>
--
hotpink :: Color
hotpink = rgba 255 105 180 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'indianred' 'Color'.
--
-- >>> renderColor indianred
-- "rgba(205,92,92,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iaW5kaWFucmVkIi8+PC9zdmc+>>
--
indianred :: Color
indianred = rgba 205 92 92 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'indigo' 'Color'.
--
-- >>> renderColor indigo
-- "rgba(75,0,130,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iaW5kaWdvIi8+PC9zdmc+>>
--
indigo :: Color
indigo = rgba 75 0 130 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'ivory' 'Color'.
--
-- >>> renderColor ivory
-- "rgba(255,255,240,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0iaXZvcnkiLz48L3N2Zz4=>>
--
ivory :: Color
ivory = rgba 255 255 240 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'khaki' 'Color'.
--
-- >>> renderColor khaki
-- "rgba(240,230,140,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ia2hha2kiLz48L3N2Zz4=>>
--
khaki :: Color
khaki = rgba 240 230 140 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lavender' 'Color'.
--
-- >>> renderColor lavender
-- "rgba(230,230,250,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGF2ZW5kZXIiLz48L3N2Zz4=>>
--
lavender :: Color
lavender = rgba 230 230 250 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lavenderblush' 'Color'.
--
-- >>> renderColor lavenderblush
-- "rgba(255,240,245,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGF2ZW5kZXJibHVzaCIvPjwvc3ZnPg==>>
--
lavenderblush :: Color
lavenderblush = rgba 255 240 245 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lawngreen' 'Color'.
--
-- >>> renderColor lawngreen
-- "rgba(124,252,0,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGF3bmdyZWVuIi8+PC9zdmc+>>
--
lawngreen :: Color
lawngreen = rgba 124 252 0 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lemonchiffon' 'Color'.
--
-- >>> renderColor lemonchiffon
-- "rgba(255,250,205,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGVtb25jaGlmZm9uIi8+PC9zdmc+>>
--
lemonchiffon :: Color
lemonchiffon = rgba 255 250 205 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lightblue' 'Color'.
--
-- >>> renderColor lightblue
-- "rgba(173,216,230,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGlnaHRibHVlIi8+PC9zdmc+>>
--
lightblue :: Color
lightblue = rgba 173 216 230 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lightcoral' 'Color'.
--
-- >>> renderColor lightcoral
-- "rgba(240,128,128,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGlnaHRjb3JhbCIvPjwvc3ZnPg==>>
--
lightcoral :: Color
lightcoral = rgba 240 128 128 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lightcyan' 'Color'.
--
-- >>> renderColor lightcyan
-- "rgba(224,255,255,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGlnaHRjeWFuIi8+PC9zdmc+>>
--
lightcyan :: Color
lightcyan = rgba 224 255 255 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lightgoldenrodyellow' 'Color'.
--
-- >>> renderColor lightgoldenrodyellow
-- "rgba(250,250,210,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGlnaHRnb2xkZW5yb2R5ZWxsb3ciLz48L3N2Zz4=>>
--
lightgoldenrodyellow :: Color
lightgoldenrodyellow = rgba 250 250 210 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lightgray' 'Color'.
--
-- >>> renderColor lightgray
-- "rgba(211,211,211,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGlnaHRncmF5Ii8+PC9zdmc+>>
--
lightgray :: Color
lightgray = rgba 211 211 211 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lightgreen' 'Color'.
--
-- >>> renderColor lightgreen
-- "rgba(144,238,144,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGlnaHRncmVlbiIvPjwvc3ZnPg==>>
--
lightgreen :: Color
lightgreen = rgba 144 238 144 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lightgrey' 'Color'.
--
-- >>> renderColor lightgrey
-- "rgba(211,211,211,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGlnaHRncmV5Ii8+PC9zdmc+>>
--
lightgrey :: Color
lightgrey = rgba 211 211 211 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lightpink' 'Color'.
--
-- >>> renderColor lightpink
-- "rgba(255,182,193,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGlnaHRwaW5rIi8+PC9zdmc+>>
--
lightpink :: Color
lightpink = rgba 255 182 193 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lightsalmon' 'Color'.
--
-- >>> renderColor lightsalmon
-- "rgba(255,160,122,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGlnaHRzYWxtb24iLz48L3N2Zz4=>>
--
lightsalmon :: Color
lightsalmon = rgba 255 160 122 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lightseagreen' 'Color'.
--
-- >>> renderColor lightseagreen
-- "rgba(32,178,170,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGlnaHRzZWFncmVlbiIvPjwvc3ZnPg==>>
--
lightseagreen :: Color
lightseagreen = rgba 32 178 170 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lightskyblue' 'Color'.
--
-- >>> renderColor lightskyblue
-- "rgba(135,206,250,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGlnaHRza3libHVlIi8+PC9zdmc+>>
--
lightskyblue :: Color
lightskyblue = rgba 135 206 250 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lightslategray' 'Color'.
--
-- >>> renderColor lightslategray
-- "rgba(119,136,153,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGlnaHRzbGF0ZWdyYXkiLz48L3N2Zz4=>>
--
lightslategray :: Color
lightslategray = rgba 119 136 153 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lightslategrey' 'Color'.
--
-- >>> renderColor lightslategrey
-- "rgba(119,136,153,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGlnaHRzbGF0ZWdyZXkiLz48L3N2Zz4=>>
--
lightslategrey :: Color
lightslategrey = rgba 119 136 153 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lightsteelblue' 'Color'.
--
-- >>> renderColor lightsteelblue
-- "rgba(176,196,222,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGlnaHRzdGVlbGJsdWUiLz48L3N2Zz4=>>
--
lightsteelblue :: Color
lightsteelblue = rgba 176 196 222 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lightyellow' 'Color'.
--
-- >>> renderColor lightyellow
-- "rgba(255,255,224,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGlnaHR5ZWxsb3ciLz48L3N2Zz4=>>
--
lightyellow :: Color
lightyellow = rgba 255 255 224 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'lime' 'Color'.
--
-- >>> renderColor lime
-- "rgba(0,255,0,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGltZSIvPjwvc3ZnPg==>>
--
lime :: Color
lime = rgba 0 255 0 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'limegreen' 'Color'.
--
-- >>> renderColor limegreen
-- "rgba(50,205,50,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGltZWdyZWVuIi8+PC9zdmc+>>
--
limegreen :: Color
limegreen = rgba 50 205 50 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'linen' 'Color'.
--
-- >>> renderColor linen
-- "rgba(250,240,230,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibGluZW4iLz48L3N2Zz4=>>
--
linen :: Color
linen = rgba 250 240 230 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'magenta' 'Color'.
--
-- >>> renderColor magenta
-- "rgba(255,0,255,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibWFnZW50YSIvPjwvc3ZnPg==>>
--
magenta :: Color
magenta = rgba 255 0 255 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'maroon' 'Color'.
--
-- >>> renderColor maroon
-- "rgba(128,0,0,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibWFyb29uIi8+PC9zdmc+>>
--
maroon :: Color
maroon = rgba 128 0 0 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'mediumaquamarine' 'Color'.
--
-- >>> renderColor mediumaquamarine
-- "rgba(102,205,170,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibWVkaXVtYXF1YW1hcmluZSIvPjwvc3ZnPg==>>
--
mediumaquamarine :: Color
mediumaquamarine = rgba 102 205 170 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'mediumblue' 'Color'.
--
-- >>> renderColor mediumblue
-- "rgba(0,0,205,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibWVkaXVtYmx1ZSIvPjwvc3ZnPg==>>
--
mediumblue :: Color
mediumblue = rgba 0 0 205 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'mediumorchid' 'Color'.
--
-- >>> renderColor mediumorchid
-- "rgba(186,85,211,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibWVkaXVtb3JjaGlkIi8+PC9zdmc+>>
--
mediumorchid :: Color
mediumorchid = rgba 186 85 211 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'mediumpurple' 'Color'.
--
-- >>> renderColor mediumpurple
-- "rgba(147,112,219,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibWVkaXVtcHVycGxlIi8+PC9zdmc+>>
--
mediumpurple :: Color
mediumpurple = rgba 147 112 219 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'mediumseagreen' 'Color'.
--
-- >>> renderColor mediumseagreen
-- "rgba(60,179,113,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibWVkaXVtc2VhZ3JlZW4iLz48L3N2Zz4=>>
--
mediumseagreen :: Color
mediumseagreen = rgba 60 179 113 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'mediumslateblue' 'Color'.
--
-- >>> renderColor mediumslateblue
-- "rgba(123,104,238,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibWVkaXVtc2xhdGVibHVlIi8+PC9zdmc+>>
--
mediumslateblue :: Color
mediumslateblue = rgba 123 104 238 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'mediumspringgreen' 'Color'.
--
-- >>> renderColor mediumspringgreen
-- "rgba(0,250,154,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibWVkaXVtc3ByaW5nZ3JlZW4iLz48L3N2Zz4=>>
--
mediumspringgreen :: Color
mediumspringgreen = rgba 0 250 154 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'mediumturquoise' 'Color'.
--
-- >>> renderColor mediumturquoise
-- "rgba(72,209,204,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibWVkaXVtdHVycXVvaXNlIi8+PC9zdmc+>>
--
mediumturquoise :: Color
mediumturquoise = rgba 72 209 204 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'mediumvioletred' 'Color'.
--
-- >>> renderColor mediumvioletred
-- "rgba(199,21,133,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibWVkaXVtdmlvbGV0cmVkIi8+PC9zdmc+>>
--
mediumvioletred :: Color
mediumvioletred = rgba 199 21 133 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'midnightblue' 'Color'.
--
-- >>> renderColor midnightblue
-- "rgba(25,25,112,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibWlkbmlnaHRibHVlIi8+PC9zdmc+>>
--
midnightblue :: Color
midnightblue = rgba 25 25 112 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'mintcream' 'Color'.
--
-- >>> renderColor mintcream
-- "rgba(245,255,250,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibWludGNyZWFtIi8+PC9zdmc+>>
--
mintcream :: Color
mintcream = rgba 245 255 250 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'mistyrose' 'Color'.
--
-- >>> renderColor mistyrose
-- "rgba(255,228,225,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibWlzdHlyb3NlIi8+PC9zdmc+>>
--
mistyrose :: Color
mistyrose = rgba 255 228 225 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'moccasin' 'Color'.
--
-- >>> renderColor moccasin
-- "rgba(255,228,181,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibW9jY2FzaW4iLz48L3N2Zz4=>>
--
moccasin :: Color
moccasin = rgba 255 228 181 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'navajowhite' 'Color'.
--
-- >>> renderColor navajowhite
-- "rgba(255,222,173,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibmF2YWpvd2hpdGUiLz48L3N2Zz4=>>
--
navajowhite :: Color
navajowhite = rgba 255 222 173 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'navy' 'Color'.
--
-- >>> renderColor navy
-- "rgba(0,0,128,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ibmF2eSIvPjwvc3ZnPg==>>
--
navy :: Color
navy = rgba 0 0 128 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'oldlace' 'Color'.
--
-- >>> renderColor oldlace
-- "rgba(253,245,230,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ib2xkbGFjZSIvPjwvc3ZnPg==>>
--
oldlace :: Color
oldlace = rgba 253 245 230 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'olive' 'Color'.
--
-- >>> renderColor olive
-- "rgba(128,128,0,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ib2xpdmUiLz48L3N2Zz4=>>
--
olive :: Color
olive = rgba 128 128 0 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'olivedrab' 'Color'.
--
-- >>> renderColor olivedrab
-- "rgba(107,142,35,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ib2xpdmVkcmFiIi8+PC9zdmc+>>
--
olivedrab :: Color
olivedrab = rgba 107 142 35 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'orange' 'Color'.
--
-- >>> renderColor orange
-- "rgba(255,165,0,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ib3JhbmdlIi8+PC9zdmc+>>
--
orange :: Color
orange = rgba 255 165 0 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'orangered' 'Color'.
--
-- >>> renderColor orangered
-- "rgba(255,69,0,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ib3JhbmdlcmVkIi8+PC9zdmc+>>
--
orangered :: Color
orangered = rgba 255 69 0 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'orchid' 'Color'.
--
-- >>> renderColor orchid
-- "rgba(218,112,214,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ib3JjaGlkIi8+PC9zdmc+>>
--
orchid :: Color
orchid = rgba 218 112 214 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'palegoldenrod' 'Color'.
--
-- >>> renderColor palegoldenrod
-- "rgba(238,232,170,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0icGFsZWdvbGRlbnJvZCIvPjwvc3ZnPg==>>
--
palegoldenrod :: Color
palegoldenrod = rgba 238 232 170 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'palegreen' 'Color'.
--
-- >>> renderColor palegreen
-- "rgba(152,251,152,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0icGFsZWdyZWVuIi8+PC9zdmc+>>
--
palegreen :: Color
palegreen = rgba 152 251 152 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'paleturquoise' 'Color'.
--
-- >>> renderColor paleturquoise
-- "rgba(175,238,238,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0icGFsZXR1cnF1b2lzZSIvPjwvc3ZnPg==>>
--
paleturquoise :: Color
paleturquoise = rgba 175 238 238 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'palevioletred' 'Color'.
--
-- >>> renderColor palevioletred
-- "rgba(219,112,147,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0icGFsZXZpb2xldHJlZCIvPjwvc3ZnPg==>>
--
palevioletred :: Color
palevioletred = rgba 219 112 147 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'papayawhip' 'Color'.
--
-- >>> renderColor papayawhip
-- "rgba(255,239,213,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0icGFwYXlhd2hpcCIvPjwvc3ZnPg==>>
--
papayawhip :: Color
papayawhip = rgba 255 239 213 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'peachpuff' 'Color'.
--
-- >>> renderColor peachpuff
-- "rgba(255,218,185,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0icGVhY2hwdWZmIi8+PC9zdmc+>>
--
peachpuff :: Color
peachpuff = rgba 255 218 185 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'peru' 'Color'.
--
-- >>> renderColor peru
-- "rgba(205,133,63,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0icGVydSIvPjwvc3ZnPg==>>
--
peru :: Color
peru = rgba 205 133 63 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'pink' 'Color'.
--
-- >>> renderColor pink
-- "rgba(255,192,203,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0icGluayIvPjwvc3ZnPg==>>
--
pink :: Color
pink = rgba 255 192 203 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'plum' 'Color'.
--
-- >>> renderColor plum
-- "rgba(221,160,221,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0icGx1bSIvPjwvc3ZnPg==>>
--
plum :: Color
plum = rgba 221 160 221 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'powderblue' 'Color'.
--
-- >>> renderColor powderblue
-- "rgba(176,224,230,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0icG93ZGVyYmx1ZSIvPjwvc3ZnPg==>>
--
powderblue :: Color
powderblue = rgba 176 224 230 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'purple' 'Color'.
--
-- >>> renderColor purple
-- "rgba(128,0,128,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0icHVycGxlIi8+PC9zdmc+>>
--
purple :: Color
purple = rgba 128 0 128 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'red' 'Color'.
--
-- >>> renderColor red
-- "rgba (255,0,0,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0icmVkIi8+PC9zdmc+>>
--
red :: Color
red = rgba 255 0 0 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'rosybrown' 'Color'.
--
-- >>> renderColor rosybrown
-- "rgba(188,143,143,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0icm9zeWJyb3duIi8+PC9zdmc+>>
--
rosybrown :: Color
rosybrown = rgba 188 143 143 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'royalblue' 'Color'.
--
-- >>> renderColor royalblue
-- "rgba(65,105,225,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0icm95YWxibHVlIi8+PC9zdmc+>>
--
royalblue :: Color
royalblue = rgba 65 105 225 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'saddlebrown' 'Color'.
--
-- >>> renderColor saddlebrown
-- "rgba(139,69,19,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ic2FkZGxlYnJvd24iLz48L3N2Zz4=>>
--
saddlebrown :: Color
saddlebrown = rgba 139 69 19 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'salmon' 'Color'.
--
-- >>> renderColor salmon
-- "rgba(250,128,114,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ic2FsbW9uIi8+PC9zdmc+>>
--
salmon :: Color
salmon = rgba 250 128 114 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'sandybrown' 'Color'.
--
-- >>> renderColor sandybrown
-- "rgba(244,164,96,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ic2FuZHlicm93biIvPjwvc3ZnPg==>>
--
sandybrown :: Color
sandybrown = rgba 244 164 96 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'seagreen' 'Color'.
--
-- >>> renderColor seagreen
-- "rgba(46,139,87,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ic2VhZ3JlZW4iLz48L3N2Zz4=>>
--
seagreen :: Color
seagreen = rgba 46 139 87 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'seashell' 'Color'.
--
-- >>> renderColor seashell
-- "rgba(255,245,238,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ic2Vhc2hlbGwiLz48L3N2Zz4=>>
--
seashell :: Color
seashell = rgba 255 245 238 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'sienna' 'Color'.
--
-- >>> renderColor sienna
-- "rgba(160,82,45,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ic2llbm5hIi8+PC9zdmc+>>
--
sienna :: Color
sienna = rgba 160 82 45 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'silver' 'Color'.
--
-- >>> renderColor silver
-- "rgba(192,192,192,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ic2lsdmVyIi8+PC9zdmc+>>
--
silver :: Color
silver = rgba 192 192 192 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'skyblue' 'Color'.
--
-- >>> renderColor skyblue
-- "rgba(135,206,235,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ic2t5Ymx1ZSIvPjwvc3ZnPg==>>
--
skyblue :: Color
skyblue = rgba 135 206 235 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'slateblue' 'Color'.
--
-- >>> renderColor slateblue
-- "rgba(106,90,205,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ic2xhdGVibHVlIi8+PC9zdmc+>>
--
slateblue :: Color
slateblue = rgba 106 90 205 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'slategray' 'Color'.
--
-- >>> renderColor slategray
-- "rgba(112,128,144,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ic2xhdGVncmF5Ii8+PC9zdmc+>>
--
slategray :: Color
slategray = rgba 112 128 144 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'slategrey' 'Color'.
--
-- >>> renderColor slategrey
-- "rgba(112,128,144,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ic2xhdGVncmV5Ii8+PC9zdmc+>>
--
slategrey :: Color
slategrey = rgba 112 128 144 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'snow' 'Color'.
--
-- >>> renderColor snow
-- "rgba(255,250,250,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ic25vdyIvPjwvc3ZnPg==>>
--
snow :: Color
snow = rgba 255 250 250 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'springgreen' 'Color'.
--
-- >>> renderColor springgreen
-- "rgba(0,255,127,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ic3ByaW5nZ3JlZW4iLz48L3N2Zz4=>>
--
springgreen :: Color
springgreen = rgba 0 255 127 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'steelblue' 'Color'.
--
-- >>> renderColor steelblue
-- "rgba(70,130,180,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ic3RlZWxibHVlIi8+PC9zdmc+>>
--
steelblue :: Color
steelblue = rgba 70 130 180 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'tan' 'Color'.
--
-- >>> renderColor tan
-- "rgba(210,180,140,1.<<data:image/svg+xml;base64,0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0idGFuIi8+PC9zdmc+>>
--
tan :: Color
tan = rgba 210 180 140 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'teal' 'Color'.
--
-- >>> renderColor teal
-- "rgba(0,128,128,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0idGVhbCIvPjwvc3ZnPg==>>
--
teal :: Color
teal = rgba 0 128 128 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'thistle' 'Color'.
--
-- >>> renderColor thistle
-- "rgba(216,191,216,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0idGhpc3RsZSIvPjwvc3ZnPg==>>
--
thistle :: Color
thistle = rgba 216 191 216 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'tomato' 'Color'.
--
-- >>> renderColor tomato
-- "rgba(255,99,71,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0idG9tYXRvIi8+PC9zdmc+>>
--
tomato :: Color
tomato = rgba 255 99 71 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'turquoise' 'Color'.
--
-- >>> renderColor turquoise
-- "rgba(64,224,208,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0idHVycXVvaXNlIi8+PC9zdmc+>>
--
turquoise :: Color
turquoise = rgba 64 224 208 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'violet' 'Color'.
--
-- >>> renderColor violet
-- "rgba(238,130,238,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0idmlvbGV0Ii8+PC9zdmc+>>
--
violet :: Color
violet = rgba 238 130 238 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'wheat' 'Color'.
--
-- >>> renderColor wheat
-- "rgba(245,222,179,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0id2hlYXQiLz48L3N2Zz4=>>
--
wheat :: Color
wheat = rgba 245 222 179 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'white' 'Color'.
--
-- >>> renderColor white
-- "rgba(255,255,255,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0id2hpdGUiLz48L3N2Zz4=>>
--
white :: Color
white = rgba 255 255 255 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'whitesmoke' 'Color'.
--
-- >>> renderColor whitesmoke
-- "rgba(245,245,245,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0id2hpdGVzbW9rZSIvPjwvc3ZnPg==>>
--
whitesmoke :: Color
whitesmoke = rgba 245 245 245 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'yellow' 'Color'.
--
-- >>> renderColor yellow
-- "rgba(255,255,0,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ieWVsbG93Ii8+PC9zdmc+>>
--
yellow :: Color
yellow = rgba 255 255 0 1
-----------------------------------------------------------------------------
-- | Smart constructor for the 'yellowgreen' 'Color'.
--
-- >>> renderColor yellowgreen
-- "rgba(154,205,50,1.0)"
--
-- <<data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIj48cmVjdCB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgZmlsbD0ieWVsbG93Z3JlZW4iLz48L3N2Zz4=>>
--
yellowgreen :: Color
yellowgreen = rgba 154 205 50 1
-----------------------------------------------------------------------------
