{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SVG elements
--
-- See: <https://developer.mozilla.org/en-US/docs/Web/SVG/Element>
module Concur.Replica.SVG where

import           Concur.Replica.DOM   (WidgetConstraints, elWithNamespace)
import           Concur.Replica.Props (Props)

import qualified Data.Text            as T

import           Replica.VDOM.Types   (Namespace(Namespace))

el :: forall m a. WidgetConstraints m => T.Text -> [Props a] -> [m a] -> m a
el = elWithNamespace (Just (Namespace "http://www.w3.org/2000/svg"))

-- * Animation

animate :: WidgetConstraints m => [Props a] -> [m a] -> m a
animate = el "animate"

animateColor :: WidgetConstraints m => [Props a] -> [m a] -> m a
animateColor = el "animate-color"

animateMotion :: WidgetConstraints m => [Props a] -> [m a] -> m a
animateMotion = el "animate-motion"

animateTransform :: WidgetConstraints m => [Props a] -> [m a] -> m a
animateTransform = el "animate-transform"

discard :: WidgetConstraints m => [Props a] -> [m a] -> m a
discard = el "discard"

mpath :: WidgetConstraints m => [Props a] -> [m a] -> m a
mpath = el "mpath"

set :: WidgetConstraints m => [Props a] -> [m a] -> m a
set = el "set"

-- * Container

a :: WidgetConstraints m => [Props a] -> [m a] -> m a
a = el "a"

defs :: WidgetConstraints m => [Props a] -> [m a] -> m a
defs = el "defs"

g :: WidgetConstraints m => [Props a] -> [m a] -> m a
g = el "g"

marker :: WidgetConstraints m => [Props a] -> [m a] -> m a
marker = el "marker"

mask :: WidgetConstraints m => [Props a] -> [m a] -> m a
mask = el "mask"

pattern :: WidgetConstraints m => [Props a] -> [m a] -> m a
pattern = el "pattern"

svg :: WidgetConstraints m => [Props a] -> [m a] -> m a
svg = el "svg"

switch :: WidgetConstraints m => [Props a] -> [m a] -> m a
switch = el "switch"

symbol :: WidgetConstraints m => [Props a] -> [m a] -> m a
symbol = el "symbol"

-- * Descriptive

desc :: WidgetConstraints m => [Props a] -> [m a] -> m a
desc = el "desc"

metadata :: WidgetConstraints m => [Props a] -> [m a] -> m a
metadata = el "metadata"

title :: WidgetConstraints m => [Props a] -> [m a] -> m a
title = el "title"

-- * Font

font :: WidgetConstraints m => [Props a] -> [m a] -> m a
font = el "font"

fontFace :: WidgetConstraints m => [Props a] -> [m a] -> m a
fontFace = el "font-face"

fontFaceFormat :: WidgetConstraints m => [Props a] -> [m a] -> m a
fontFaceFormat = el "font-face-format"

fontFaceName :: WidgetConstraints m => [Props a] -> [m a] -> m a
fontFaceName = el "font-face-name"

fontFaceSrc :: WidgetConstraints m => [Props a] -> [m a] -> m a
fontFaceSrc = el "font-face-src"

fontFaceUri :: WidgetConstraints m => [Props a] -> [m a] -> m a
fontFaceUri = el "font-face-uri"

hkern :: WidgetConstraints m => [Props a] -> [m a] -> m a
hkern = el "hkern"

vkern :: WidgetConstraints m => [Props a] -> [m a] -> m a
vkern = el "vkern"
-- * Graphics

circle :: WidgetConstraints m => [Props a] -> [m a] -> m a
circle = el "circle"

ellipse :: WidgetConstraints m => [Props a] -> [m a] -> m a
ellipse = el "ellipse"

image :: WidgetConstraints m => [Props a] -> [m a] -> m a
image = el "image"

line :: WidgetConstraints m => [Props a] -> [m a] -> m a
line = el "line"

path :: WidgetConstraints m => [Props a] -> [m a] -> m a
path = el "path"

polygon :: WidgetConstraints m => [Props a] -> [m a] -> m a
polygon = el "polygon"

polyline :: WidgetConstraints m => [Props a] -> [m a] -> m a
polyline = el "polyline"

rect :: WidgetConstraints m => [Props a] -> [m a] -> m a
rect = el "rect"

text :: WidgetConstraints m => [Props a] -> [m a] -> m a
text = el "text"

use :: WidgetConstraints m => [Props a] -> [m a] -> m a
use = el "use"
