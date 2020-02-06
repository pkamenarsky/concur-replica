{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings #-}

module Concur.Replica.DOM.Props where

import qualified Data.Text                as T

import           Replica.VDOM             (DOMEvent)

data Prop a
  = PropText T.Text
  | PropBool Bool
  | PropEvent (DOMEvent -> a)
  | PropMap [Props a]
  deriving Functor

data Props a = Props T.Text (Prop a)
  deriving Functor

boolProp :: T.Text -> Bool -> Props a
boolProp k v = Props k (PropBool v)

textProp :: T.Text -> T.Text -> Props a
textProp k v = Props k (PropText v)

key :: T.Text -> Props a
key v = textProp "key" v

style :: [(T.Text, T.Text)] -> Props a
style m = Props "style" (PropMap [ Props k (PropText v) | (k, v) <- m ])

-- | Define multiple classes conditionally
--
-- > div [ classList [ ("empty", null items) ] [ ]
--
classList ::  [(T.Text, Bool)] -> Props a
classList xs = textProp "class" $ T.intercalate " " [ t | (t, True) <- xs ]

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/title>
title ::  T.Text -> Props a
title = textProp "title"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/selected>
selected ::  Bool -> Props a
selected = boolProp "selected"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/hidden>
hidden ::  Bool -> Props a
hidden             = boolProp "hidden"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/value>
value ::  T.Text -> Props a
value             = textProp "value"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/defaultValue>
defaultValue ::  T.Text -> Props a
defaultValue      = textProp "defaultValue"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/accept>
accept ::  T.Text -> Props a
accept            = textProp "accept"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/acceptCharset>
acceptCharset ::  T.Text -> Props a
acceptCharset     = textProp "acceptCharset"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/a>
a_ ::  T.Text -> Props a
a_            = textProp "a"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/autocomplete>
autocomplete ::  Bool -> Props a
autocomplete b = textProp "autocomplete" (if b then "on" else "off")

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/autosave>
autosave ::  T.Text -> Props a
autosave          = textProp "autosave"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/disabled>
disabled ::  Bool -> Props a
disabled          = boolProp "disabled"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/enctype>
enctype ::  T.Text -> Props a
enctype           = textProp "enctype"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/formation>
formation ::  T.Text -> Props a
formation         = textProp "formation"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/list>
list ::  T.Text -> Props a
list              = textProp "list"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/maxlength>
maxlength ::  T.Text -> Props a
maxlength         = textProp "maxlength"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/minlength>
minlength ::  T.Text -> Props a
minlength         = textProp "minlength"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/method>
method ::  T.Text -> Props a
method            = textProp "method"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/multiple>
multiple ::  Bool -> Props a
multiple          = boolProp "multiple"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/novalidate>
novalidate ::  Bool -> Props a
novalidate        = boolProp "noValidate"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/pattern>
pattern ::  T.Text -> Props a
pattern           = textProp "pattern"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/readonly>
readonly ::  Bool -> Props a
readonly          = boolProp "readOnly"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/required>
required ::  Bool -> Props a
required          = boolProp "required"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/size>
size ::  T.Text -> Props a
size              = textProp "size"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/for>
for ::  T.Text -> Props a
for               = textProp "for"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/form>
form_ ::  T.Text -> Props a
form_               = textProp "form"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/max>
max ::  T.Text -> Props a
max               = textProp "max"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/min>
min ::  T.Text -> Props a
min               = textProp "min"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/step>
step ::  T.Text -> Props a
step              = textProp "step"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/cols>
cols ::  T.Text -> Props a
cols              = textProp "cols"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/rows>
rows ::  T.Text -> Props a
rows              = textProp "rows"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/wrap>
wrap ::  T.Text -> Props a
wrap              = textProp "wrap"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/target>
target_ ::  T.Text -> Props a
target_            = textProp "target"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/download>
download ::  T.Text -> Props a
download          = textProp "download"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/downloadAs>
downloadAs ::  T.Text -> Props a
downloadAs        = textProp "downloadAs"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/hreflang>
hreflang ::  T.Text -> Props a
hreflang          = textProp "hreflang"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/media>
media ::  T.Text -> Props a
media             = textProp "media"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/ping>
ping ::  T.Text -> Props a
ping              = textProp "ping"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/rel>
rel ::  T.Text -> Props a
rel               = textProp "rel"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/ismap>
ismap ::  T.Text -> Props a
ismap             = textProp "ismap"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/usemap>
usemap ::  T.Text -> Props a
usemap            = textProp "usemap"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/shape>
shape ::  T.Text -> Props a
shape             = textProp "shape"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/coords>
coords ::  T.Text -> Props a
coords            = textProp "coords"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/src>
src ::  T.Text -> Props a
src               = textProp "src"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/height>
height ::  T.Text -> Props a
height            = textProp "height"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/width>
width ::  T.Text -> Props a
width             = textProp "width"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/alt>
alt ::  T.Text -> Props a
alt               = textProp "alt"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/autoplay>
autoplay ::  Bool -> Props a
autoplay          = boolProp "autoplay"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/controls>
controls ::  Bool -> Props a
controls          = boolProp "controls"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/loop>
loop ::  Bool -> Props a
loop              = boolProp "loop"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/preload>
preload ::  T.Text -> Props a
preload           = textProp "preload"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/poster>
poster ::  T.Text -> Props a
poster            = textProp "poster"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/default>
default_ ::  Bool -> Props a
default_           = boolProp "default"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/kind>
kind ::  T.Text -> Props a
kind              = textProp "kind"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/srclang>
srclang ::  T.Text -> Props a
srclang           = textProp "srclang"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/sandbox>
sandbox ::  T.Text -> Props a
sandbox           = textProp "sandbox"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/seamless>
seamless ::  T.Text -> Props a
seamless          = textProp "seamless"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/srcdoc>
srcdoc ::  T.Text -> Props a
srcdoc            = textProp "srcdoc"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/reversed>
reversed ::  T.Text -> Props a
reversed          = textProp "reversed"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/start>
start ::  T.Text -> Props a
start             = textProp "start"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/align>
align ::  T.Text -> Props a
align             = textProp "align"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/colspan>
colspan ::  T.Text -> Props a
colspan           = textProp "colspan"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/rowspan>
rowspan ::  T.Text -> Props a
rowspan           = textProp "rowspan"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/headers>
headers ::  T.Text -> Props a
headers           = textProp "headers"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/scope>
scope ::  T.Text -> Props a
scope             = textProp "scope"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/async>
async ::  T.Text -> Props a
async             = textProp "async"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/charset>
charset ::  T.Text -> Props a
charset           = textProp "charset"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/content>
content ::  T.Text -> Props a
content           = textProp "content"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/defer>
defer ::  T.Text -> Props a
defer             = textProp "defer"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/httpEquiv>
httpEquiv ::  T.Text -> Props a
httpEquiv         = textProp "httpEquiv"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/language>
language ::  T.Text -> Props a
language          = textProp "language"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/scoped>
scoped ::  T.Text -> Props a
scoped            = textProp "scoped"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/type>
type_ ::  T.Text -> Props a
type_ = textProp "type"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/name>
name ::  T.Text -> Props a
name = textProp "name"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/href>
href ::  T.Text -> Props a
href = textProp "href"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/id>
id ::  T.Text -> Props a
id = textProp "id"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/placeholder>
placeholder ::  T.Text -> Props a
placeholder = textProp "placeholder"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/checked>
checked ::  Bool -> Props a
checked = boolProp "checked"

-- | Set "autofocus" property
-- <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/autofocus>
autofocus ::  Bool -> Props a
autofocus = boolProp "autofocus"

-- | Set "className" property
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/className>
className ::  T.Text -> Props a
className = textProp "class"
