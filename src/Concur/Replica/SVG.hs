{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SVG elements
--
-- See: <https://developer.mozilla.org/en-US/docs/Web/SVG/Element>
module Concur.Replica.SVG where

-- Note that this module is auto-generated.
-- See @./misc/gen-svg-modules@ for details.

import           Concur.Replica.DOM       (WidgetConstraints, elWithNamespace)
import           Concur.Replica.DOM.Props (Props)

import qualified Data.Text                as T

import           Replica.VDOM.Types       (Namespace(Namespace))

-- | Helper function for creating SVG elements.
el :: forall m a. WidgetConstraints m => T.Text -> [Props a] -> [m a] -> m a
el = elWithNamespace (Just (Namespace "http://www.w3.org/2000/svg"))

-- * SVG Elements

animate :: WidgetConstraints m => [Props a] -> [m a] -> m a
animate = el "animate"

animateMotion :: WidgetConstraints m => [Props a] -> [m a] -> m a
animateMotion = el "animateMotion"

animateTransform :: WidgetConstraints m => [Props a] -> [m a] -> m a
animateTransform = el "animateTransform"

circle :: WidgetConstraints m => [Props a] -> [m a] -> m a
circle = el "circle"

clipPath :: WidgetConstraints m => [Props a] -> [m a] -> m a
clipPath = el "clipPath"

colorProfile :: WidgetConstraints m => [Props a] -> [m a] -> m a
colorProfile = el "color-profile"

defs :: WidgetConstraints m => [Props a] -> [m a] -> m a
defs = el "defs"

desc :: WidgetConstraints m => [Props a] -> [m a] -> m a
desc = el "desc"

discard :: WidgetConstraints m => [Props a] -> [m a] -> m a
discard = el "discard"

ellipse :: WidgetConstraints m => [Props a] -> [m a] -> m a
ellipse = el "ellipse"

feBlend :: WidgetConstraints m => [Props a] -> [m a] -> m a
feBlend = el "feBlend"

feColorMatrix :: WidgetConstraints m => [Props a] -> [m a] -> m a
feColorMatrix = el "feColorMatrix"

feComponentTransfer :: WidgetConstraints m => [Props a] -> [m a] -> m a
feComponentTransfer = el "feComponentTransfer"

feComposite :: WidgetConstraints m => [Props a] -> [m a] -> m a
feComposite = el "feComposite"

feConvolveMatrix :: WidgetConstraints m => [Props a] -> [m a] -> m a
feConvolveMatrix = el "feConvolveMatrix"

feDiffuseLighting :: WidgetConstraints m => [Props a] -> [m a] -> m a
feDiffuseLighting = el "feDiffuseLighting"

feDisplacementMap :: WidgetConstraints m => [Props a] -> [m a] -> m a
feDisplacementMap = el "feDisplacementMap"

feDistantLight :: WidgetConstraints m => [Props a] -> [m a] -> m a
feDistantLight = el "feDistantLight"

feDropShadow :: WidgetConstraints m => [Props a] -> [m a] -> m a
feDropShadow = el "feDropShadow"

feFlood :: WidgetConstraints m => [Props a] -> [m a] -> m a
feFlood = el "feFlood"

feFuncA :: WidgetConstraints m => [Props a] -> [m a] -> m a
feFuncA = el "feFuncA"

feFuncB :: WidgetConstraints m => [Props a] -> [m a] -> m a
feFuncB = el "feFuncB"

feFuncG :: WidgetConstraints m => [Props a] -> [m a] -> m a
feFuncG = el "feFuncG"

feFuncR :: WidgetConstraints m => [Props a] -> [m a] -> m a
feFuncR = el "feFuncR"

feGaussianBlur :: WidgetConstraints m => [Props a] -> [m a] -> m a
feGaussianBlur = el "feGaussianBlur"

feImage :: WidgetConstraints m => [Props a] -> [m a] -> m a
feImage = el "feImage"

feMerge :: WidgetConstraints m => [Props a] -> [m a] -> m a
feMerge = el "feMerge"

feMergeNode :: WidgetConstraints m => [Props a] -> [m a] -> m a
feMergeNode = el "feMergeNode"

feMorphology :: WidgetConstraints m => [Props a] -> [m a] -> m a
feMorphology = el "feMorphology"

feOffset :: WidgetConstraints m => [Props a] -> [m a] -> m a
feOffset = el "feOffset"

fePointLight :: WidgetConstraints m => [Props a] -> [m a] -> m a
fePointLight = el "fePointLight"

feSpecularLighting :: WidgetConstraints m => [Props a] -> [m a] -> m a
feSpecularLighting = el "feSpecularLighting"

feSpotLight :: WidgetConstraints m => [Props a] -> [m a] -> m a
feSpotLight = el "feSpotLight"

feTile :: WidgetConstraints m => [Props a] -> [m a] -> m a
feTile = el "feTile"

feTurbulence :: WidgetConstraints m => [Props a] -> [m a] -> m a
feTurbulence = el "feTurbulence"

filter :: WidgetConstraints m => [Props a] -> [m a] -> m a
filter = el "filter"

foreignObject :: WidgetConstraints m => [Props a] -> [m a] -> m a
foreignObject = el "foreignObject"

g :: WidgetConstraints m => [Props a] -> [m a] -> m a
g = el "g"

hatch :: WidgetConstraints m => [Props a] -> [m a] -> m a
hatch = el "hatch"

hatchpath :: WidgetConstraints m => [Props a] -> [m a] -> m a
hatchpath = el "hatchpath"

image :: WidgetConstraints m => [Props a] -> [m a] -> m a
image = el "image"

line :: WidgetConstraints m => [Props a] -> [m a] -> m a
line = el "line"

linearGradient :: WidgetConstraints m => [Props a] -> [m a] -> m a
linearGradient = el "linearGradient"

marker :: WidgetConstraints m => [Props a] -> [m a] -> m a
marker = el "marker"

mask :: WidgetConstraints m => [Props a] -> [m a] -> m a
mask = el "mask"

mesh :: WidgetConstraints m => [Props a] -> [m a] -> m a
mesh = el "mesh"

meshgradient :: WidgetConstraints m => [Props a] -> [m a] -> m a
meshgradient = el "meshgradient"

meshpatch :: WidgetConstraints m => [Props a] -> [m a] -> m a
meshpatch = el "meshpatch"

meshrow :: WidgetConstraints m => [Props a] -> [m a] -> m a
meshrow = el "meshrow"

metadata :: WidgetConstraints m => [Props a] -> [m a] -> m a
metadata = el "metadata"

mpath :: WidgetConstraints m => [Props a] -> [m a] -> m a
mpath = el "mpath"

path :: WidgetConstraints m => [Props a] -> [m a] -> m a
path = el "path"

pattern :: WidgetConstraints m => [Props a] -> [m a] -> m a
pattern = el "pattern"

polygon :: WidgetConstraints m => [Props a] -> [m a] -> m a
polygon = el "polygon"

polyline :: WidgetConstraints m => [Props a] -> [m a] -> m a
polyline = el "polyline"

radialGradient :: WidgetConstraints m => [Props a] -> [m a] -> m a
radialGradient = el "radialGradient"

rect :: WidgetConstraints m => [Props a] -> [m a] -> m a
rect = el "rect"

script :: WidgetConstraints m => [Props a] -> [m a] -> m a
script = el "script"

set :: WidgetConstraints m => [Props a] -> [m a] -> m a
set = el "set"

solidcolor :: WidgetConstraints m => [Props a] -> [m a] -> m a
solidcolor = el "solidcolor"

stop :: WidgetConstraints m => [Props a] -> [m a] -> m a
stop = el "stop"

style :: WidgetConstraints m => [Props a] -> [m a] -> m a
style = el "style"

svg :: WidgetConstraints m => [Props a] -> [m a] -> m a
svg = el "svg"

switch :: WidgetConstraints m => [Props a] -> [m a] -> m a
switch = el "switch"

symbol :: WidgetConstraints m => [Props a] -> [m a] -> m a
symbol = el "symbol"

text :: WidgetConstraints m => [Props a] -> [m a] -> m a
text = el "text"

textPath :: WidgetConstraints m => [Props a] -> [m a] -> m a
textPath = el "textPath"

title :: WidgetConstraints m => [Props a] -> [m a] -> m a
title = el "title"

tspan :: WidgetConstraints m => [Props a] -> [m a] -> m a
tspan = el "tspan"

unknown :: WidgetConstraints m => [Props a] -> [m a] -> m a
unknown = el "unknown"

use :: WidgetConstraints m => [Props a] -> [m a] -> m a
use = el "use"

view :: WidgetConstraints m => [Props a] -> [m a] -> m a
view = el "view"
