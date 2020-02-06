{-# LANGUAGE OverloadedStrings #-}

-- | SVG properties/attributes
--
-- See: <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute>
module Concur.Replica.SVG.Props where

-- Note that this module is auto-generated.
-- See @./misc/gen-svg-modules@ for details.

import           Concur.Replica.DOM.Props (Props, textProp)

import qualified Data.Text                as T

accentHeight :: T.Text -> Props a
accentHeight = textProp "accent-height"

accumulate :: T.Text -> Props a
accumulate = textProp "accumulate"

additive :: T.Text -> Props a
additive = textProp "additive"

alignmentBaseline :: T.Text -> Props a
alignmentBaseline = textProp "alignment-baseline"

allowReorder :: T.Text -> Props a
allowReorder = textProp "allowReorder"

alphabetic :: T.Text -> Props a
alphabetic = textProp "alphabetic"

amplitude :: T.Text -> Props a
amplitude = textProp "amplitude"

arabicForm :: T.Text -> Props a
arabicForm = textProp "arabic-form"

ascent :: T.Text -> Props a
ascent = textProp "ascent"

attributeName :: T.Text -> Props a
attributeName = textProp "attributeName"

attributeType :: T.Text -> Props a
attributeType = textProp "attributeType"

autoReverse :: T.Text -> Props a
autoReverse = textProp "autoReverse"

azimuth :: T.Text -> Props a
azimuth = textProp "azimuth"

baseFrequency :: T.Text -> Props a
baseFrequency = textProp "baseFrequency"

baselineShift :: T.Text -> Props a
baselineShift = textProp "baseline-shift"

baseProfile :: T.Text -> Props a
baseProfile = textProp "baseProfile"

bbox :: T.Text -> Props a
bbox = textProp "bbox"

begin :: T.Text -> Props a
begin = textProp "begin"

bias :: T.Text -> Props a
bias = textProp "bias"

by :: T.Text -> Props a
by = textProp "by"

calcMode :: T.Text -> Props a
calcMode = textProp "calcMode"

capHeight :: T.Text -> Props a
capHeight = textProp "cap-height"

className :: T.Text -> Props a
className = textProp "class"

clip :: T.Text -> Props a
clip = textProp "clip"

clipPathUnits :: T.Text -> Props a
clipPathUnits = textProp "clipPathUnits"

clipPath :: T.Text -> Props a
clipPath = textProp "clip-path"

clipRule :: T.Text -> Props a
clipRule = textProp "clip-rule"

color :: T.Text -> Props a
color = textProp "color"

colorInterpolation :: T.Text -> Props a
colorInterpolation = textProp "color-interpolation"

colorInterpolationFilters :: T.Text -> Props a
colorInterpolationFilters = textProp "color-interpolation-filters"

colorProfile :: T.Text -> Props a
colorProfile = textProp "color-profile"

colorRendering :: T.Text -> Props a
colorRendering = textProp "color-rendering"

contentScriptType :: T.Text -> Props a
contentScriptType = textProp "contentScriptType"

contentStyleType :: T.Text -> Props a
contentStyleType = textProp "contentStyleType"

cursor :: T.Text -> Props a
cursor = textProp "cursor"

cx :: T.Text -> Props a
cx = textProp "cx"

cy :: T.Text -> Props a
cy = textProp "cy"

d :: T.Text -> Props a
d = textProp "d"

decelerate :: T.Text -> Props a
decelerate = textProp "decelerate"

descent :: T.Text -> Props a
descent = textProp "descent"

diffuseConstant :: T.Text -> Props a
diffuseConstant = textProp "diffuseConstant"

direction :: T.Text -> Props a
direction = textProp "direction"

display :: T.Text -> Props a
display = textProp "display"

divisor :: T.Text -> Props a
divisor = textProp "divisor"

dominantBaseline :: T.Text -> Props a
dominantBaseline = textProp "dominant-baseline"

dur :: T.Text -> Props a
dur = textProp "dur"

dx :: T.Text -> Props a
dx = textProp "dx"

dy :: T.Text -> Props a
dy = textProp "dy"

edgeMode :: T.Text -> Props a
edgeMode = textProp "edgeMode"

elevation :: T.Text -> Props a
elevation = textProp "elevation"

enableBackground :: T.Text -> Props a
enableBackground = textProp "enable-background"

end :: T.Text -> Props a
end = textProp "end"

exponent :: T.Text -> Props a
exponent = textProp "exponent"

externalResourcesRequired :: T.Text -> Props a
externalResourcesRequired = textProp "externalResourcesRequired"

fill :: T.Text -> Props a
fill = textProp "fill"

fillOpacity :: T.Text -> Props a
fillOpacity = textProp "fill-opacity"

fillRule :: T.Text -> Props a
fillRule = textProp "fill-rule"

filter :: T.Text -> Props a
filter = textProp "filter"

filterRes :: T.Text -> Props a
filterRes = textProp "filterRes"

filterUnits :: T.Text -> Props a
filterUnits = textProp "filterUnits"

floodColor :: T.Text -> Props a
floodColor = textProp "flood-color"

floodOpacity :: T.Text -> Props a
floodOpacity = textProp "flood-opacity"

fontFamily :: T.Text -> Props a
fontFamily = textProp "font-family"

fontSize :: T.Text -> Props a
fontSize = textProp "font-size"

fontSizeAdjust :: T.Text -> Props a
fontSizeAdjust = textProp "font-size-adjust"

fontStretch :: T.Text -> Props a
fontStretch = textProp "font-stretch"

fontStyle :: T.Text -> Props a
fontStyle = textProp "font-style"

fontVariant :: T.Text -> Props a
fontVariant = textProp "font-variant"

fontWeight :: T.Text -> Props a
fontWeight = textProp "font-weight"

format :: T.Text -> Props a
format = textProp "format"

from :: T.Text -> Props a
from = textProp "from"

fr :: T.Text -> Props a
fr = textProp "fr"

fx :: T.Text -> Props a
fx = textProp "fx"

fy :: T.Text -> Props a
fy = textProp "fy"

g1 :: T.Text -> Props a
g1 = textProp "g1"

g2 :: T.Text -> Props a
g2 = textProp "g2"

glyphName :: T.Text -> Props a
glyphName = textProp "glyph-name"

glyphOrientationHorizontal :: T.Text -> Props a
glyphOrientationHorizontal = textProp "glyph-orientation-horizontal"

glyphOrientationVertical :: T.Text -> Props a
glyphOrientationVertical = textProp "glyph-orientation-vertical"

glyphRef :: T.Text -> Props a
glyphRef = textProp "glyphRef"

gradientTransform :: T.Text -> Props a
gradientTransform = textProp "gradientTransform"

gradientUnits :: T.Text -> Props a
gradientUnits = textProp "gradientUnits"

hanging :: T.Text -> Props a
hanging = textProp "hanging"

height :: T.Text -> Props a
height = textProp "height"

href :: T.Text -> Props a
href = textProp "href"

hreflang :: T.Text -> Props a
hreflang = textProp "hreflang"

horizAdvX :: T.Text -> Props a
horizAdvX = textProp "horiz-adv-x"

horizOriginX :: T.Text -> Props a
horizOriginX = textProp "horiz-origin-x"

id :: T.Text -> Props a
id = textProp "id"

ideographic :: T.Text -> Props a
ideographic = textProp "ideographic"

imageRendering :: T.Text -> Props a
imageRendering = textProp "image-rendering"

in_ :: T.Text -> Props a
in_ = textProp "in"

in2 :: T.Text -> Props a
in2 = textProp "in2"

intercept :: T.Text -> Props a
intercept = textProp "intercept"

k :: T.Text -> Props a
k = textProp "k"

k1 :: T.Text -> Props a
k1 = textProp "k1"

k2 :: T.Text -> Props a
k2 = textProp "k2"

k3 :: T.Text -> Props a
k3 = textProp "k3"

k4 :: T.Text -> Props a
k4 = textProp "k4"

kernelMatrix :: T.Text -> Props a
kernelMatrix = textProp "kernelMatrix"

kernelUnitLength :: T.Text -> Props a
kernelUnitLength = textProp "kernelUnitLength"

kerning :: T.Text -> Props a
kerning = textProp "kerning"

keyPoints :: T.Text -> Props a
keyPoints = textProp "keyPoints"

keySplines :: T.Text -> Props a
keySplines = textProp "keySplines"

keyTimes :: T.Text -> Props a
keyTimes = textProp "keyTimes"

lang :: T.Text -> Props a
lang = textProp "lang"

lengthAdjust :: T.Text -> Props a
lengthAdjust = textProp "lengthAdjust"

letterSpacing :: T.Text -> Props a
letterSpacing = textProp "letter-spacing"

lightingColor :: T.Text -> Props a
lightingColor = textProp "lighting-color"

limitingConeAngle :: T.Text -> Props a
limitingConeAngle = textProp "limitingConeAngle"

local :: T.Text -> Props a
local = textProp "local"

markerEnd :: T.Text -> Props a
markerEnd = textProp "marker-end"

markerMid :: T.Text -> Props a
markerMid = textProp "marker-mid"

markerStart :: T.Text -> Props a
markerStart = textProp "marker-start"

markerHeight :: T.Text -> Props a
markerHeight = textProp "markerHeight"

markerUnits :: T.Text -> Props a
markerUnits = textProp "markerUnits"

markerWidth :: T.Text -> Props a
markerWidth = textProp "markerWidth"

mask :: T.Text -> Props a
mask = textProp "mask"

maskContentUnits :: T.Text -> Props a
maskContentUnits = textProp "maskContentUnits"

maskUnits :: T.Text -> Props a
maskUnits = textProp "maskUnits"

mathematical :: T.Text -> Props a
mathematical = textProp "mathematical"

max :: T.Text -> Props a
max = textProp "max"

media :: T.Text -> Props a
media = textProp "media"

method :: T.Text -> Props a
method = textProp "method"

min :: T.Text -> Props a
min = textProp "min"

mode :: T.Text -> Props a
mode = textProp "mode"

name :: T.Text -> Props a
name = textProp "name"

numOctaves :: T.Text -> Props a
numOctaves = textProp "numOctaves"

offset :: T.Text -> Props a
offset = textProp "offset"

opacity :: T.Text -> Props a
opacity = textProp "opacity"

operator :: T.Text -> Props a
operator = textProp "operator"

order :: T.Text -> Props a
order = textProp "order"

orient :: T.Text -> Props a
orient = textProp "orient"

orientation :: T.Text -> Props a
orientation = textProp "orientation"

origin :: T.Text -> Props a
origin = textProp "origin"

overflow :: T.Text -> Props a
overflow = textProp "overflow"

overlinePosition :: T.Text -> Props a
overlinePosition = textProp "overline-position"

overlineThickness :: T.Text -> Props a
overlineThickness = textProp "overline-thickness"

panose1 :: T.Text -> Props a
panose1 = textProp "panose-1"

paintOrder :: T.Text -> Props a
paintOrder = textProp "paint-order"

path :: T.Text -> Props a
path = textProp "path"

pathLength :: T.Text -> Props a
pathLength = textProp "pathLength"

patternContentUnits :: T.Text -> Props a
patternContentUnits = textProp "patternContentUnits"

patternTransform :: T.Text -> Props a
patternTransform = textProp "patternTransform"

patternUnits :: T.Text -> Props a
patternUnits = textProp "patternUnits"

ping :: T.Text -> Props a
ping = textProp "ping"

pointerEvents :: T.Text -> Props a
pointerEvents = textProp "pointer-events"

points :: T.Text -> Props a
points = textProp "points"

pointsAtX :: T.Text -> Props a
pointsAtX = textProp "pointsAtX"

pointsAtY :: T.Text -> Props a
pointsAtY = textProp "pointsAtY"

pointsAtZ :: T.Text -> Props a
pointsAtZ = textProp "pointsAtZ"

preserveAlpha :: T.Text -> Props a
preserveAlpha = textProp "preserveAlpha"

preserveAspectRatio :: T.Text -> Props a
preserveAspectRatio = textProp "preserveAspectRatio"

primitiveUnits :: T.Text -> Props a
primitiveUnits = textProp "primitiveUnits"

r :: T.Text -> Props a
r = textProp "r"

radius :: T.Text -> Props a
radius = textProp "radius"

referrerPolicy :: T.Text -> Props a
referrerPolicy = textProp "referrerPolicy"

refX :: T.Text -> Props a
refX = textProp "refX"

refY :: T.Text -> Props a
refY = textProp "refY"

rel :: T.Text -> Props a
rel = textProp "rel"

renderingIntent :: T.Text -> Props a
renderingIntent = textProp "rendering-intent"

repeatCount :: T.Text -> Props a
repeatCount = textProp "repeatCount"

repeatDur :: T.Text -> Props a
repeatDur = textProp "repeatDur"

requiredExtensions :: T.Text -> Props a
requiredExtensions = textProp "requiredExtensions"

requiredFeatures :: T.Text -> Props a
requiredFeatures = textProp "requiredFeatures"

restart :: T.Text -> Props a
restart = textProp "restart"

result :: T.Text -> Props a
result = textProp "result"

rotate :: T.Text -> Props a
rotate = textProp "rotate"

rx :: T.Text -> Props a
rx = textProp "rx"

ry :: T.Text -> Props a
ry = textProp "ry"

scale :: T.Text -> Props a
scale = textProp "scale"

seed :: T.Text -> Props a
seed = textProp "seed"

shapeRendering :: T.Text -> Props a
shapeRendering = textProp "shape-rendering"

slope :: T.Text -> Props a
slope = textProp "slope"

spacing :: T.Text -> Props a
spacing = textProp "spacing"

specularConstant :: T.Text -> Props a
specularConstant = textProp "specularConstant"

specularExponent :: T.Text -> Props a
specularExponent = textProp "specularExponent"

speed :: T.Text -> Props a
speed = textProp "speed"

spreadMethod :: T.Text -> Props a
spreadMethod = textProp "spreadMethod"

startOffset :: T.Text -> Props a
startOffset = textProp "startOffset"

stdDeviation :: T.Text -> Props a
stdDeviation = textProp "stdDeviation"

stemh :: T.Text -> Props a
stemh = textProp "stemh"

stemv :: T.Text -> Props a
stemv = textProp "stemv"

stitchTiles :: T.Text -> Props a
stitchTiles = textProp "stitchTiles"

stopColor :: T.Text -> Props a
stopColor = textProp "stop-color"

stopOpacity :: T.Text -> Props a
stopOpacity = textProp "stop-opacity"

strikethroughPosition :: T.Text -> Props a
strikethroughPosition = textProp "strikethrough-position"

strikethroughThickness :: T.Text -> Props a
strikethroughThickness = textProp "strikethrough-thickness"

string :: T.Text -> Props a
string = textProp "string"

stroke :: T.Text -> Props a
stroke = textProp "stroke"

strokeDasharray :: T.Text -> Props a
strokeDasharray = textProp "stroke-dasharray"

strokeDashoffset :: T.Text -> Props a
strokeDashoffset = textProp "stroke-dashoffset"

strokeLinecap :: T.Text -> Props a
strokeLinecap = textProp "stroke-linecap"

strokeLinejoin :: T.Text -> Props a
strokeLinejoin = textProp "stroke-linejoin"

strokeMiterlimit :: T.Text -> Props a
strokeMiterlimit = textProp "stroke-miterlimit"

strokeOpacity :: T.Text -> Props a
strokeOpacity = textProp "stroke-opacity"

strokeWidth :: T.Text -> Props a
strokeWidth = textProp "stroke-width"

style :: T.Text -> Props a
style = textProp "style"

surfaceScale :: T.Text -> Props a
surfaceScale = textProp "surfaceScale"

systemLanguage :: T.Text -> Props a
systemLanguage = textProp "systemLanguage"

tabindex :: T.Text -> Props a
tabindex = textProp "tabindex"

tableValues :: T.Text -> Props a
tableValues = textProp "tableValues"

target :: T.Text -> Props a
target = textProp "target"

targetX :: T.Text -> Props a
targetX = textProp "targetX"

targetY :: T.Text -> Props a
targetY = textProp "targetY"

textAnchor :: T.Text -> Props a
textAnchor = textProp "text-anchor"

textDecoration :: T.Text -> Props a
textDecoration = textProp "text-decoration"

textRendering :: T.Text -> Props a
textRendering = textProp "text-rendering"

textLength :: T.Text -> Props a
textLength = textProp "textLength"

to :: T.Text -> Props a
to = textProp "to"

transform :: T.Text -> Props a
transform = textProp "transform"

type_ :: T.Text -> Props a
type_ = textProp "type"

u1 :: T.Text -> Props a
u1 = textProp "u1"

u2 :: T.Text -> Props a
u2 = textProp "u2"

underlinePosition :: T.Text -> Props a
underlinePosition = textProp "underline-position"

underlineThickness :: T.Text -> Props a
underlineThickness = textProp "underline-thickness"

unicode :: T.Text -> Props a
unicode = textProp "unicode"

unicodeBidi :: T.Text -> Props a
unicodeBidi = textProp "unicode-bidi"

unicodeRange :: T.Text -> Props a
unicodeRange = textProp "unicode-range"

unitsPerEm :: T.Text -> Props a
unitsPerEm = textProp "units-per-em"

vAlphabetic :: T.Text -> Props a
vAlphabetic = textProp "v-alphabetic"

vHanging :: T.Text -> Props a
vHanging = textProp "v-hanging"

vIdeographic :: T.Text -> Props a
vIdeographic = textProp "v-ideographic"

vMathematical :: T.Text -> Props a
vMathematical = textProp "v-mathematical"

values :: T.Text -> Props a
values = textProp "values"

vectorEffect :: T.Text -> Props a
vectorEffect = textProp "vector-effect"

version :: T.Text -> Props a
version = textProp "version"

vertAdvY :: T.Text -> Props a
vertAdvY = textProp "vert-adv-y"

vertOriginX :: T.Text -> Props a
vertOriginX = textProp "vert-origin-x"

vertOriginY :: T.Text -> Props a
vertOriginY = textProp "vert-origin-y"

viewBox :: T.Text -> Props a
viewBox = textProp "viewBox"

viewTarget :: T.Text -> Props a
viewTarget = textProp "viewTarget"

visibility :: T.Text -> Props a
visibility = textProp "visibility"

width :: T.Text -> Props a
width = textProp "width"

widths :: T.Text -> Props a
widths = textProp "widths"

wordSpacing :: T.Text -> Props a
wordSpacing = textProp "word-spacing"

writingMode :: T.Text -> Props a
writingMode = textProp "writing-mode"

x :: T.Text -> Props a
x = textProp "x"

xHeight :: T.Text -> Props a
xHeight = textProp "x-height"

x1 :: T.Text -> Props a
x1 = textProp "x1"

x2 :: T.Text -> Props a
x2 = textProp "x2"

xChannelSelector :: T.Text -> Props a
xChannelSelector = textProp "xChannelSelector"

xlinkActuate :: T.Text -> Props a
xlinkActuate = textProp "xlink:actuate"

xlinkArcrole :: T.Text -> Props a
xlinkArcrole = textProp "xlink:arcrole"

xlinkHref :: T.Text -> Props a
xlinkHref = textProp "xlink:href"

xlinkRole :: T.Text -> Props a
xlinkRole = textProp "xlink:role"

xlinkShow :: T.Text -> Props a
xlinkShow = textProp "xlink:show"

xlinkTitle :: T.Text -> Props a
xlinkTitle = textProp "xlink:title"

xlinkType :: T.Text -> Props a
xlinkType = textProp "xlink:type"

xmlBase :: T.Text -> Props a
xmlBase = textProp "xml:base"

xmlLang :: T.Text -> Props a
xmlLang = textProp "xml:lang"

xmlSpace :: T.Text -> Props a
xmlSpace = textProp "xml:space"

y :: T.Text -> Props a
y = textProp "y"

y1 :: T.Text -> Props a
y1 = textProp "y1"

y2 :: T.Text -> Props a
y2 = textProp "y2"

yChannelSelector :: T.Text -> Props a
yChannelSelector = textProp "yChannelSelector"

z :: T.Text -> Props a
z = textProp "z"

zoomAndPan :: T.Text -> Props a
zoomAndPan = textProp "zoomAndPan"
