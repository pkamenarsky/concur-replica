{-# LANGUAGE OverloadedStrings   #-}

-- | SVG elements
--
-- See: <https://developer.mozilla.org/en-US/docs/Web/SVG/Element>
module Concur.Replica.SVG where

-- Note that this module is auto-generated.
-- See @./misc/gen-svg-modules@ for details.

import           Concur.Replica.UI        (UI)

import           Concur.Replica.DOM       (elWithNamespace)
import           Concur.Replica.DOM.Props (Props)

import qualified Data.Text                as T

import           Replica.VDOM             (HTML)
import           Replica.VDOM.Types       (Namespace(Namespace))

-- | Helper function for creating SVG elements.
el :: T.Text -> [Props a] -> [UI HTML a] -> UI HTML a
el = elWithNamespace (Just (Namespace "http://www.w3.org/2000/svg"))

-- * SVG Elements

animate :: [Props a] -> [UI HTML a] -> UI HTML a
animate = el "animate"

animateMotion :: [Props a] -> [UI HTML a] -> UI HTML a
animateMotion = el "animateMotion"

animateTransform :: [Props a] -> [UI HTML a] -> UI HTML a
animateTransform = el "animateTransform"

circle :: [Props a] -> [UI HTML a] -> UI HTML a
circle = el "circle"

clipPath :: [Props a] -> [UI HTML a] -> UI HTML a
clipPath = el "clipPath"

colorProfile :: [Props a] -> [UI HTML a] -> UI HTML a
colorProfile = el "color-profile"

defs :: [Props a] -> [UI HTML a] -> UI HTML a
defs = el "defs"

desc :: [Props a] -> [UI HTML a] -> UI HTML a
desc = el "desc"

discard :: [Props a] -> [UI HTML a] -> UI HTML a
discard = el "discard"

ellipse :: [Props a] -> [UI HTML a] -> UI HTML a
ellipse = el "ellipse"

feBlend :: [Props a] -> [UI HTML a] -> UI HTML a
feBlend = el "feBlend"

feColorMatrix :: [Props a] -> [UI HTML a] -> UI HTML a
feColorMatrix = el "feColorMatrix"

feComponentTransfer :: [Props a] -> [UI HTML a] -> UI HTML a
feComponentTransfer = el "feComponentTransfer"

feComposite :: [Props a] -> [UI HTML a] -> UI HTML a
feComposite = el "feComposite"

feConvolveMatrix :: [Props a] -> [UI HTML a] -> UI HTML a
feConvolveMatrix = el "feConvolveMatrix"

feDiffuseLighting :: [Props a] -> [UI HTML a] -> UI HTML a
feDiffuseLighting = el "feDiffuseLighting"

feDisplacementMap :: [Props a] -> [UI HTML a] -> UI HTML a
feDisplacementMap = el "feDisplacementMap"

feDistantLight :: [Props a] -> [UI HTML a] -> UI HTML a
feDistantLight = el "feDistantLight"

feDropShadow :: [Props a] -> [UI HTML a] -> UI HTML a
feDropShadow = el "feDropShadow"

feFlood :: [Props a] -> [UI HTML a] -> UI HTML a
feFlood = el "feFlood"

feFuncA :: [Props a] -> [UI HTML a] -> UI HTML a
feFuncA = el "feFuncA"

feFuncB :: [Props a] -> [UI HTML a] -> UI HTML a
feFuncB = el "feFuncB"

feFuncG :: [Props a] -> [UI HTML a] -> UI HTML a
feFuncG = el "feFuncG"

feFuncR :: [Props a] -> [UI HTML a] -> UI HTML a
feFuncR = el "feFuncR"

feGaussianBlur :: [Props a] -> [UI HTML a] -> UI HTML a
feGaussianBlur = el "feGaussianBlur"

feImage :: [Props a] -> [UI HTML a] -> UI HTML a
feImage = el "feImage"

feMerge :: [Props a] -> [UI HTML a] -> UI HTML a
feMerge = el "feMerge"

feMergeNode :: [Props a] -> [UI HTML a] -> UI HTML a
feMergeNode = el "feMergeNode"

feMorphology :: [Props a] -> [UI HTML a] -> UI HTML a
feMorphology = el "feMorphology"

feOffset :: [Props a] -> [UI HTML a] -> UI HTML a
feOffset = el "feOffset"

fePointLight :: [Props a] -> [UI HTML a] -> UI HTML a
fePointLight = el "fePointLight"

feSpecularLighting :: [Props a] -> [UI HTML a] -> UI HTML a
feSpecularLighting = el "feSpecularLighting"

feSpotLight :: [Props a] -> [UI HTML a] -> UI HTML a
feSpotLight = el "feSpotLight"

feTile :: [Props a] -> [UI HTML a] -> UI HTML a
feTile = el "feTile"

feTurbulence :: [Props a] -> [UI HTML a] -> UI HTML a
feTurbulence = el "feTurbulence"

filter :: [Props a] -> [UI HTML a] -> UI HTML a
filter = el "filter"

foreignObject :: [Props a] -> [UI HTML a] -> UI HTML a
foreignObject = el "foreignObject"

g :: [Props a] -> [UI HTML a] -> UI HTML a
g = el "g"

hatch :: [Props a] -> [UI HTML a] -> UI HTML a
hatch = el "hatch"

hatchpath :: [Props a] -> [UI HTML a] -> UI HTML a
hatchpath = el "hatchpath"

image :: [Props a] -> [UI HTML a] -> UI HTML a
image = el "image"

line :: [Props a] -> [UI HTML a] -> UI HTML a
line = el "line"

linearGradient :: [Props a] -> [UI HTML a] -> UI HTML a
linearGradient = el "linearGradient"

marker :: [Props a] -> [UI HTML a] -> UI HTML a
marker = el "marker"

mask :: [Props a] -> [UI HTML a] -> UI HTML a
mask = el "mask"

mesh :: [Props a] -> [UI HTML a] -> UI HTML a
mesh = el "mesh"

meshgradient :: [Props a] -> [UI HTML a] -> UI HTML a
meshgradient = el "meshgradient"

meshpatch :: [Props a] -> [UI HTML a] -> UI HTML a
meshpatch = el "meshpatch"

meshrow :: [Props a] -> [UI HTML a] -> UI HTML a
meshrow = el "meshrow"

metadata :: [Props a] -> [UI HTML a] -> UI HTML a
metadata = el "metadata"

mpath :: [Props a] -> [UI HTML a] -> UI HTML a
mpath = el "mpath"

path :: [Props a] -> [UI HTML a] -> UI HTML a
path = el "path"

pattern :: [Props a] -> [UI HTML a] -> UI HTML a
pattern = el "pattern"

polygon :: [Props a] -> [UI HTML a] -> UI HTML a
polygon = el "polygon"

polyline :: [Props a] -> [UI HTML a] -> UI HTML a
polyline = el "polyline"

radialGradient :: [Props a] -> [UI HTML a] -> UI HTML a
radialGradient = el "radialGradient"

rect :: [Props a] -> [UI HTML a] -> UI HTML a
rect = el "rect"

script :: [Props a] -> [UI HTML a] -> UI HTML a
script = el "script"

set :: [Props a] -> [UI HTML a] -> UI HTML a
set = el "set"

solidcolor :: [Props a] -> [UI HTML a] -> UI HTML a
solidcolor = el "solidcolor"

stop :: [Props a] -> [UI HTML a] -> UI HTML a
stop = el "stop"

style :: [Props a] -> [UI HTML a] -> UI HTML a
style = el "style"

svg :: [Props a] -> [UI HTML a] -> UI HTML a
svg = el "svg"

switch :: [Props a] -> [UI HTML a] -> UI HTML a
switch = el "switch"

symbol :: [Props a] -> [UI HTML a] -> UI HTML a
symbol = el "symbol"

text :: [Props a] -> [UI HTML a] -> UI HTML a
text = el "text"

textPath :: [Props a] -> [UI HTML a] -> UI HTML a
textPath = el "textPath"

title :: [Props a] -> [UI HTML a] -> UI HTML a
title = el "title"

tspan :: [Props a] -> [UI HTML a] -> UI HTML a
tspan = el "tspan"

unknown :: [Props a] -> [UI HTML a] -> UI HTML a
unknown = el "unknown"

use :: [Props a] -> [UI HTML a] -> UI HTML a
use = el "use"

view :: [Props a] -> [UI HTML a] -> UI HTML a
view = el "view"
