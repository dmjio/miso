-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Style.Color
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Miso.Style.Color
  ( -- *** Types
    Color (RGBA, HSL, Hex)
    -- *** Smart constructor
  , rgba
  , hsl
  , hex
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
import           Miso.String (MisoString)
import qualified Miso.String as MS
-----------------------------------------------------------------------------
import           Prelude hiding (tan)
-----------------------------------------------------------------------------
data Color
  = RGBA Int Int Int Int
  | HSL Int Int Int
  | Hex MisoString
  deriving (Show, Eq)
-----------------------------------------------------------------------------
renderColor :: Color -> MisoString
renderColor (RGBA r g b a) = "rgba(" <> values <> ")"
  where
    values = MS.intercalate ","
      [ MS.ms r
      , MS.ms g
      , MS.ms b
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
-----------------------------------------------------------------------------
-- | 'rgba'
--
rgba :: Int -> Int -> Int -> Int -> Color
rgba = RGBA
-----------------------------------------------------------------------------
-- | 'hsl'
--
hsl :: Int -> Int -> Int -> Color
hsl = HSL
-----------------------------------------------------------------------------
-- | 'hex'
--
hex :: MisoString -> Color
hex = Hex
-----------------------------------------------------------------------------
-- | 'transparent'
--
transparent :: Color
transparent = rgba 0 0 0 0
-----------------------------------------------------------------------------
-- | 'aliceblue'
--
aliceblue :: Color
aliceblue = rgba 240 248 255 1
-----------------------------------------------------------------------------
-- | 'antiquewhite'
--
antiquewhite :: Color
antiquewhite = rgba 250 235 215 1
-----------------------------------------------------------------------------
-- | 'aqua'
--
aqua :: Color
aqua = rgba 0 255 255 1
-----------------------------------------------------------------------------
-- | 'aquamarine'
--
aquamarine :: Color
aquamarine = rgba 127 255 212 1
-----------------------------------------------------------------------------
-- | 'azure'
--
azure :: Color
azure = rgba 240 255 255 1
-----------------------------------------------------------------------------
-- | 'beige'
--
beige :: Color
beige = rgba 245 245 220 1
-----------------------------------------------------------------------------
-- | 'bisque'
--
bisque :: Color
bisque = rgba 255 228 196 1
-----------------------------------------------------------------------------
-- | 'black'
--
black :: Color
black = rgba 0 0 0 1
-----------------------------------------------------------------------------
-- | 'blanchedalmond'
--
blanchedalmond :: Color
blanchedalmond = rgba 255 235 205 1
-----------------------------------------------------------------------------
-- | 'blue'
--
blue :: Color
blue = rgba 0 0 255 1
-----------------------------------------------------------------------------
-- | 'blueviolet'
--
blueviolet :: Color
blueviolet = rgba 138 43 226 1
-----------------------------------------------------------------------------
-- | 'brown'
--
brown :: Color
brown = rgba 165 42 42 1
-----------------------------------------------------------------------------
-- | 'burlywood'
--
burlywood :: Color
burlywood = rgba 222 184 135 1
-----------------------------------------------------------------------------
-- | 'cadetblue'
--
cadetblue :: Color
cadetblue = rgba 95 158 160 1
-----------------------------------------------------------------------------
-- | 'chartreuse'
--
chartreuse :: Color
chartreuse = rgba 127 255 0 1
-----------------------------------------------------------------------------
-- | 'chocolate'
--
chocolate :: Color
chocolate = rgba 210 105 30 1
-----------------------------------------------------------------------------
-- | 'coral'
--
coral :: Color
coral = rgba 255 127 80 1
-----------------------------------------------------------------------------
-- | 'cornflowerblue'
--
cornflowerblue :: Color
cornflowerblue = rgba 100 149 237 1
-----------------------------------------------------------------------------
-- | 'cornsilk'
--
cornsilk :: Color
cornsilk = rgba 255 248 220 1
-----------------------------------------------------------------------------
-- | 'crimson'
--
crimson :: Color
crimson = rgba 220 20 60 1
-----------------------------------------------------------------------------
-- | 'cyan'
--
cyan :: Color
cyan = rgba 0 255 255 1
-----------------------------------------------------------------------------
-- | 'darkblue'
--
darkblue :: Color
darkblue = rgba 0 0 139 1
-----------------------------------------------------------------------------
-- | 'darkcyan'
--
darkcyan :: Color
darkcyan = rgba 0 139 139 1
-----------------------------------------------------------------------------
-- | 'darkgoldenrod'
--
darkgoldenrod :: Color
darkgoldenrod = rgba 184 134 11 1
-----------------------------------------------------------------------------
-- | 'darkgray'
--
darkgray :: Color
darkgray = rgba 169 169 169 1
-----------------------------------------------------------------------------
-- | 'darkgreen'
--
darkgreen :: Color
darkgreen = rgba 0 100 0 1
-----------------------------------------------------------------------------
-- | 'darkgrey'
--
darkgrey :: Color
darkgrey = rgba 169 169 169 1
-----------------------------------------------------------------------------
-- | 'darkkhaki'
--
darkkhaki :: Color
darkkhaki = rgba 189 183 107 1
-----------------------------------------------------------------------------
-- | 'darkmagenta'
--
darkmagenta :: Color
darkmagenta = rgba 139 0 139 1
-----------------------------------------------------------------------------
-- | 'darkolivegreen'
--
darkolivegreen :: Color
darkolivegreen = rgba 85 107 47 1
-----------------------------------------------------------------------------
-- | 'darkorange'
--
darkorange :: Color
darkorange = rgba 255 140 0 1
-----------------------------------------------------------------------------
-- | 'darkorchid'
--
darkorchid :: Color
darkorchid = rgba 153 50 204 1
-----------------------------------------------------------------------------
-- | 'darkred'
--
darkred :: Color
darkred = rgba 139 0 0 1
-----------------------------------------------------------------------------
-- | 'darksalmon'
--
darksalmon :: Color
darksalmon = rgba 233 150 122 1
-----------------------------------------------------------------------------
-- | 'darkseagreen'
--
darkseagreen :: Color
darkseagreen = rgba 143 188 143 1
-----------------------------------------------------------------------------
-- | 'darkslateblue'
--
darkslateblue :: Color
darkslateblue = rgba 72 61 139 1
-----------------------------------------------------------------------------
-- | 'darkslategray'
--
darkslategray :: Color
darkslategray = rgba 47 79 79 1
-----------------------------------------------------------------------------
-- | 'darkslategrey'
--
darkslategrey :: Color
darkslategrey = rgba 47 79 79 1
-----------------------------------------------------------------------------
-- | 'darkturquoise'
--
darkturquoise :: Color
darkturquoise = rgba 0 206 209 1
-----------------------------------------------------------------------------
-- | 'darkviolet'
--
darkviolet :: Color
darkviolet = rgba 148 0 211 1
-----------------------------------------------------------------------------
-- | 'deeppink'
--
deeppink :: Color
deeppink = rgba 255 20 147 1
-----------------------------------------------------------------------------
-- | 'deepskyblue'
--
deepskyblue :: Color
deepskyblue = rgba 0 191 255 1
-----------------------------------------------------------------------------
-- | 'dimgray'
--
dimgray :: Color
dimgray = rgba 105 105 105 1
-----------------------------------------------------------------------------
-- | 'dimgrey'
--
dimgrey :: Color
dimgrey = rgba 105 105 105 1
-----------------------------------------------------------------------------
-- | 'dodgerblue'
--
dodgerblue :: Color
dodgerblue = rgba 30 144 255 1
-----------------------------------------------------------------------------
-- | 'firebrick'
--
firebrick :: Color
firebrick = rgba 178 34 34 1
-----------------------------------------------------------------------------
-- | 'floralwhite'
--
floralwhite :: Color
floralwhite = rgba 255 250 240 1
-----------------------------------------------------------------------------
-- | 'forestgreen'
--
forestgreen :: Color
forestgreen = rgba 34 139 34 1
-----------------------------------------------------------------------------
-- | 'fuchsia'
--
fuchsia :: Color
fuchsia = rgba 255 0 255 1
-----------------------------------------------------------------------------
-- | 'gainsboro'
--
gainsboro :: Color
gainsboro = rgba 220 220 220 1
-----------------------------------------------------------------------------
-- | 'ghostwhite'
--
ghostwhite :: Color
ghostwhite = rgba 248 248 255 1
-----------------------------------------------------------------------------
-- | 'gold'
--
gold :: Color
gold = rgba 255 215 0 1
-----------------------------------------------------------------------------
-- | 'goldenrod'
--
goldenrod :: Color
goldenrod = rgba 218 165 32 1
-----------------------------------------------------------------------------
-- | 'gray'
--
gray :: Color
gray = rgba 128 128 128 1
-----------------------------------------------------------------------------
-- | 'green'
--
green :: Color
green = rgba 0 128 0 1
-----------------------------------------------------------------------------
-- | 'greenyellow'
--
greenyellow :: Color
greenyellow = rgba 173 255 47 1
-----------------------------------------------------------------------------
-- | 'grey'
--
grey :: Color
grey = rgba 128 128 128 1
-----------------------------------------------------------------------------
-- | 'honeydew'
--
honeydew :: Color
honeydew = rgba 240 255 240 1
-----------------------------------------------------------------------------
-- | 'hotpink'
--
hotpink :: Color
hotpink = rgba 255 105 180 1
-----------------------------------------------------------------------------
-- | 'indianred'
--
indianred :: Color
indianred = rgba 205 92 92 1
-----------------------------------------------------------------------------
-- | 'indigo'
--
indigo :: Color
indigo = rgba 75 0 130 1
-----------------------------------------------------------------------------
-- | 'ivory'
--
ivory :: Color
ivory = rgba 255 255 240 1
-----------------------------------------------------------------------------
-- | 'khaki'
--
khaki :: Color
khaki = rgba 240 230 140 1
-----------------------------------------------------------------------------
-- | 'lavender'
--
lavender :: Color
lavender = rgba 230 230 250 1
-----------------------------------------------------------------------------
-- | 'lavenderblush'
--
lavenderblush :: Color
lavenderblush = rgba 255 240 245 1
-----------------------------------------------------------------------------
-- | 'lawngreen'
--
lawngreen :: Color
lawngreen = rgba 124 252 0 1
-----------------------------------------------------------------------------
-- | 'lemonchiffon'
--
lemonchiffon :: Color
lemonchiffon = rgba 255 250 205 1
-----------------------------------------------------------------------------
-- | 'lightblue'
--
lightblue :: Color
lightblue = rgba 173 216 230 1
-----------------------------------------------------------------------------
-- | 'lightcoral'
--
lightcoral :: Color
lightcoral = rgba 240 128 128 1
-----------------------------------------------------------------------------
-- | 'lightcyan'
--
lightcyan :: Color
lightcyan = rgba 224 255 255 1
-----------------------------------------------------------------------------
-- | 'lightgoldenrodyellow'
--
lightgoldenrodyellow :: Color
lightgoldenrodyellow = rgba 250 250 210 1
-----------------------------------------------------------------------------
-- | 'lightgray'
--
lightgray :: Color
lightgray = rgba 211 211 211 1
-----------------------------------------------------------------------------
-- | 'lightgreen'
--
lightgreen :: Color
lightgreen = rgba 144 238 144 1
-----------------------------------------------------------------------------
-- | 'lightgrey'
--
lightgrey :: Color
lightgrey = rgba 211 211 211 1
-----------------------------------------------------------------------------
-- | 'lightpink'
--
lightpink :: Color
lightpink = rgba 255 182 193 1
-----------------------------------------------------------------------------
-- | 'lightsalmon'
--
lightsalmon :: Color
lightsalmon = rgba 255 160 122 1
-----------------------------------------------------------------------------
-- | 'lightseagreen'
--
lightseagreen :: Color
lightseagreen = rgba 32 178 170 1
-----------------------------------------------------------------------------
-- | 'lightskyblue'
--
lightskyblue :: Color
lightskyblue = rgba 135 206 250 1
-----------------------------------------------------------------------------
-- | 'lightslategray'
--
lightslategray :: Color
lightslategray = rgba 119 136 153 1
-----------------------------------------------------------------------------
-- | 'lightslategrey'
--
lightslategrey :: Color
lightslategrey = rgba 119 136 153 1
-----------------------------------------------------------------------------
-- | 'lightsteelblue'
--
lightsteelblue :: Color
lightsteelblue = rgba 176 196 222 1
-----------------------------------------------------------------------------
-- | 'lightyellow'
--
lightyellow :: Color
lightyellow = rgba 255 255 224 1
-----------------------------------------------------------------------------
-- | 'lime'
--
lime :: Color
lime = rgba 0 255 0 1
-----------------------------------------------------------------------------
-- | 'limegreen'
--
limegreen :: Color
limegreen = rgba 50 205 50 1
-----------------------------------------------------------------------------
-- | 'linen'
--
linen :: Color
linen = rgba 250 240 230 1
-----------------------------------------------------------------------------
-- | 'magenta'
--
magenta :: Color
magenta = rgba 255 0 255 1
-----------------------------------------------------------------------------
-- | 'maroon'
--
maroon :: Color
maroon = rgba 128 0 0 1
-----------------------------------------------------------------------------
-- | 'mediumaquamarine'
--
mediumaquamarine :: Color
mediumaquamarine = rgba 102 205 170 1
-----------------------------------------------------------------------------
-- | 'mediumblue'
--
mediumblue :: Color
mediumblue = rgba 0 0 205 1
-----------------------------------------------------------------------------
-- | 'mediumorchid'
--
mediumorchid :: Color
mediumorchid = rgba 186 85 211 1
-----------------------------------------------------------------------------
-- | 'mediumpurple'
--
mediumpurple :: Color
mediumpurple = rgba 147 112 219 1
-----------------------------------------------------------------------------
-- | 'mediumseagreen'
--
mediumseagreen :: Color
mediumseagreen = rgba 60 179 113 1
-----------------------------------------------------------------------------
-- | 'mediumslateblue'
--
mediumslateblue :: Color
mediumslateblue = rgba 123 104 238 1
-----------------------------------------------------------------------------
-- | 'mediumspringgreen'
--
mediumspringgreen :: Color
mediumspringgreen = rgba 0 250 154 1
-----------------------------------------------------------------------------
-- | 'mediumturquoise'
--
mediumturquoise :: Color
mediumturquoise = rgba 72 209 204 1
-----------------------------------------------------------------------------
-- | 'mediumvioletred'
--
mediumvioletred :: Color
mediumvioletred = rgba 199 21 133 1
-----------------------------------------------------------------------------
-- | 'midnightblue'
--
midnightblue :: Color
midnightblue = rgba 25 25 112 1
-----------------------------------------------------------------------------
-- | 'mintcream'
--
mintcream :: Color
mintcream = rgba 245 255 250 1
-----------------------------------------------------------------------------
-- | 'mistyrose'
--
mistyrose :: Color
mistyrose = rgba 255 228 225 1
-----------------------------------------------------------------------------
-- | 'moccasin'
--
moccasin :: Color
moccasin = rgba 255 228 181 1
-----------------------------------------------------------------------------
-- | 'navajowhite'
--
navajowhite :: Color
navajowhite = rgba 255 222 173 1
-----------------------------------------------------------------------------
-- | 'navy'
--
navy :: Color
navy = rgba 0 0 128 1
-----------------------------------------------------------------------------
-- | 'oldlace'
--
oldlace :: Color
oldlace = rgba 253 245 230 1
-----------------------------------------------------------------------------
-- | 'olive'
--
olive :: Color
olive = rgba 128 128 0 1
-----------------------------------------------------------------------------
-- | 'olivedrab'
--
olivedrab :: Color
olivedrab = rgba 107 142 35 1
-----------------------------------------------------------------------------
-- | 'orange'
--
orange :: Color
orange = rgba 255 165 0 1
-----------------------------------------------------------------------------
-- | 'orangered'
--
orangered :: Color
orangered = rgba 255 69 0 1
-----------------------------------------------------------------------------
-- | 'orchid'
--
orchid :: Color
orchid = rgba 218 112 214 1
-----------------------------------------------------------------------------
-- | 'palegoldenrod'
--
palegoldenrod :: Color
palegoldenrod = rgba 238 232 170 1
-----------------------------------------------------------------------------
-- | 'palegreen'
--
palegreen :: Color
palegreen = rgba 152 251 152 1
-----------------------------------------------------------------------------
-- | 'paleturquoise'
--
paleturquoise :: Color
paleturquoise = rgba 175 238 238 1
-----------------------------------------------------------------------------
-- | 'palevioletred'
--
palevioletred :: Color
palevioletred = rgba 219 112 147 1
-----------------------------------------------------------------------------
-- | 'papayawhip'
--
papayawhip :: Color
papayawhip = rgba 255 239 213 1
-----------------------------------------------------------------------------
-- | 'peachpuff'
--
peachpuff :: Color
peachpuff = rgba 255 218 185 1
-----------------------------------------------------------------------------
-- | 'peru'
--
peru :: Color
peru = rgba 205 133 63 1
-----------------------------------------------------------------------------
-- | 'pink'
--
pink :: Color
pink = rgba 255 192 203 1
-----------------------------------------------------------------------------
-- | 'plum'
--
plum :: Color
plum = rgba 221 160 221 1
-----------------------------------------------------------------------------
-- | 'powderblue'
--
powderblue :: Color
powderblue = rgba 176 224 230 1
-----------------------------------------------------------------------------
-- | 'purple'
--
purple :: Color
purple = rgba 128 0 128 1
-----------------------------------------------------------------------------
-- | 'red'
--
red :: Color
red = rgba 255 0 0 1
-----------------------------------------------------------------------------
-- | 'rosybrown'
--
rosybrown :: Color
rosybrown = rgba 188 143 143 1
-----------------------------------------------------------------------------
-- | 'royalblue'
--
royalblue :: Color
royalblue = rgba 65 105 225 1
-----------------------------------------------------------------------------
-- | 'saddlebrown'
--
saddlebrown :: Color
saddlebrown = rgba 139 69 19 1
-----------------------------------------------------------------------------
-- | 'salmon'
--
salmon :: Color
salmon = rgba 250 128 114 1
-----------------------------------------------------------------------------
-- | 'sandybrown'
--
sandybrown :: Color
sandybrown = rgba 244 164 96 1
-----------------------------------------------------------------------------
-- | 'seagreen'
--
seagreen :: Color
seagreen = rgba 46 139 87 1
-----------------------------------------------------------------------------
-- | 'seashell'
--
seashell :: Color
seashell = rgba 255 245 238 1
-----------------------------------------------------------------------------
-- | 'sienna'
--
sienna :: Color
sienna = rgba 160 82 45 1
-----------------------------------------------------------------------------
-- | 'silver'
--
silver :: Color
silver = rgba 192 192 192 1
-----------------------------------------------------------------------------
-- | 'skyblue'
--
skyblue :: Color
skyblue = rgba 135 206 235 1
-----------------------------------------------------------------------------
-- | 'slateblue'
--
slateblue :: Color
slateblue = rgba 106 90 205 1
-----------------------------------------------------------------------------
-- | 'slategray'
--
slategray :: Color
slategray = rgba 112 128 144 1
-----------------------------------------------------------------------------
-- | 'slategrey'
--
slategrey :: Color
slategrey = rgba 112 128 144 1
-----------------------------------------------------------------------------
-- | 'snow'
--
snow :: Color
snow = rgba 255 250 250 1
-----------------------------------------------------------------------------
-- | 'springgreen'
--
springgreen :: Color
springgreen = rgba 0 255 127 1
-----------------------------------------------------------------------------
-- | 'steelblue'
--
steelblue :: Color
steelblue = rgba 70 130 180 1
-----------------------------------------------------------------------------
-- | 'tan'
--
tan :: Color
tan = rgba 210 180 140 1
-----------------------------------------------------------------------------
-- | 'teal'
--
teal :: Color
teal = rgba 0 128 128 1
-----------------------------------------------------------------------------
-- | 'thistle'
--
thistle :: Color
thistle = rgba 216 191 216 1
-----------------------------------------------------------------------------
-- | 'tomato'
--
tomato :: Color
tomato = rgba 255 99 71 1
-----------------------------------------------------------------------------
-- | 'turquoise'
--
turquoise :: Color
turquoise = rgba 64 224 208 1
-----------------------------------------------------------------------------
-- | 'violet'
--
violet :: Color
violet = rgba 238 130 238 1
-----------------------------------------------------------------------------
-- | 'wheat'
--
wheat :: Color
wheat = rgba 245 222 179 1
-----------------------------------------------------------------------------
-- | 'white'
--
white :: Color
white = rgba 255 255 255 1
-----------------------------------------------------------------------------
-- | 'whitesmoke'
--
whitesmoke :: Color
whitesmoke = rgba 245 245 245 1
-----------------------------------------------------------------------------
-- | 'yellow'
--
yellow :: Color
yellow = rgba 255 255 0 1
-----------------------------------------------------------------------------
-- | 'yellowgreen'
--
yellowgreen :: Color
yellowgreen = rgba 154 205 50 1
-----------------------------------------------------------------------------
