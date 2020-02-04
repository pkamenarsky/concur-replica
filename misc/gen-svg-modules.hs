#!/usr/bin/env stack
-- stack --resolver lts-13.19 script --ghc-options=-Wall
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Regenerate the SVG attributes module.
--
-- Use from the top level of the repo, not the ./misc folder.
--
-- Eg:
--
--   $ ./misc/gen-svg-props.hs
module Main where

import Data.String.QQ
import Data.Text (Text)
import Data.Text.Encoding

import qualified Data.ByteString as BS
import qualified Data.Char as Char
import qualified Data.Text as T


main :: IO ()
main =
  BS.writeFile "./src/Concur/Replica/SVG/Props.hs" (encodeUtf8 generatedFile)


generatedFile :: Text
generatedFile =
  T.intercalate "\n" (fileStart : (uncurry attributeFunction <$> attributeMap))


attributeFunction :: Text -> Text -> Text
attributeFunction nameHs nameSvg =
     nameHs <> " :: T.Text -> Props a\n"
  <> nameHs <> " = textProp \"" <> nameSvg <> "\"\n"


attributeMap :: [(Text, Text)]
attributeMap =
  let
    svgNames :: [Text]
    svgNames = filter (/= mempty) (T.splitOn "\n" attributeList)

    camelCase :: Text -> Text
    camelCase t =
      let
        f :: (Bool, Text) -> Char -> (Bool, Text)
        f (capitalizeNext, acc) c
          | c == ':' || c == '-' = (True, acc)
          | capitalizeNext       = (False, T.snoc acc (Char.toUpper c))
          | otherwise            = (False, T.snoc acc c)
      in
        snd (T.foldl' f (False, mempty) t)

    toHaskellName :: Text -> Text
    toHaskellName t =
      case camelCase t of
        "class" ->
          "class'"

        "in" ->
          "in'"

        "type" ->
          "type'"

        other ->
          other
  in
    (\svgName -> (toHaskellName svgName, svgName)) <$> svgNames


fileStart :: Text
fileStart = [s|
{-# LANGUAGE OverloadedStrings #-}

-- | Note that this module is auto-generated.
-- See @./misc/generate-svg-props@ for details.
module Concur.Replica.SVG.Props where

import           Concur.Replica.Props (Props, textProp)

import qualified Data.Text            as T
|]


-- | From here: https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute
--
-- Copy-pasted it from the HTML page and then removed the "A", "B", "C", etc
-- alphabetical headings.
attributeList :: Text
attributeList = [s|
accent-height
accumulate
additive
alignment-baseline
allowReorder
alphabetic
amplitude
arabic-form
ascent
attributeName
attributeType
autoReverse
azimuth
baseFrequency
baseline-shift
baseProfile
bbox
begin
bias
by
calcMode
cap-height
class
clip
clipPathUnits
clip-path
clip-rule
color
color-interpolation
color-interpolation-filters
color-profile
color-rendering
contentScriptType
contentStyleType
cursor
cx
cy
d
decelerate
descent
diffuseConstant
direction
display
divisor
dominant-baseline
dur
dx
dy
edgeMode
elevation
enable-background
end
exponent
externalResourcesRequired
fill
fill-opacity
fill-rule
filter
filterRes
filterUnits
flood-color
flood-opacity
font-family
font-size
font-size-adjust
font-stretch
font-style
font-variant
font-weight
format
from
fr
fx
fy
g1
g2
glyph-name
glyph-orientation-horizontal
glyph-orientation-vertical
glyphRef
gradientTransform
gradientUnits
hanging
height
href
hreflang
horiz-adv-x
horiz-origin-x
id
ideographic
image-rendering
in
in2
intercept
k
k1
k2
k3
k4
kernelMatrix
kernelUnitLength
kerning
keyPoints
keySplines
keyTimes
lang
lengthAdjust
letter-spacing
lighting-color
limitingConeAngle
local
marker-end
marker-mid
marker-start
markerHeight
markerUnits
markerWidth
mask
maskContentUnits
maskUnits
mathematical
max
media
method
min
mode
name
numOctaves
offset
opacity
operator
order
orient
orientation
origin
overflow
overline-position
overline-thickness
panose-1
paint-order
path
pathLength
patternContentUnits
patternTransform
patternUnits
ping
pointer-events
points
pointsAtX
pointsAtY
pointsAtZ
preserveAlpha
preserveAspectRatio
primitiveUnits
r
radius
referrerPolicy
refX
refY
rel
rendering-intent
repeatCount
repeatDur
requiredExtensions
requiredFeatures
restart
result
rotate
rx
ry
scale
seed
shape-rendering
slope
spacing
specularConstant
specularExponent
speed
spreadMethod
startOffset
stdDeviation
stemh
stemv
stitchTiles
stop-color
stop-opacity
strikethrough-position
strikethrough-thickness
string
stroke
stroke-dasharray
stroke-dashoffset
stroke-linecap
stroke-linejoin
stroke-miterlimit
stroke-opacity
stroke-width
style
surfaceScale
systemLanguage
tabindex
tableValues
target
targetX
targetY
text-anchor
text-decoration
text-rendering
textLength
to
transform
type
u1
u2
underline-position
underline-thickness
unicode
unicode-bidi
unicode-range
units-per-em
v-alphabetic
v-hanging
v-ideographic
v-mathematical
values
vector-effect
version
vert-adv-y
vert-origin-x
vert-origin-y
viewBox
viewTarget
visibility
width
widths
word-spacing
writing-mode
x
x-height
x1
x2
xChannelSelector
xlink:actuate
xlink:arcrole
xlink:href
xlink:role
xlink:show
xlink:title
xlink:type
xml:base
xml:lang
xml:space
y
y1
y2
yChannelSelector
z
zoomAndPan
|]
