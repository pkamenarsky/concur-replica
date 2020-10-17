{-# LANGUAGE OverloadedStrings   #-}

module Concur.Replica.DOM where

import           Control.Concurrent.STM       (STM, atomically)
import           Control.Concurrent.STM.TMVar (newEmptyTMVar, putTMVar, takeTMVar)

import           Concur.Replica.DOM.Props     (Props(Props), Prop(PropText, PropBool, PropEvent, PropMap), key)

import           Concur.Replica.UI            (UI)
import qualified Concur.Replica.UI            as UI

import qualified Data.Text                    as T
import qualified Data.Map                     as M

import           Replica.VDOM                 (Attrs, Attr(AText, ABool, AEvent, AMap), HTML, Namespace, VDOM(VNode, VLeaf, VText))

el :: T.Text -> [Props a] -> [UI HTML a] -> UI HTML a
el = elWithNamespace Nothing

elWithNamespace :: Maybe Namespace -> T.Text -> [Props a] -> [UI HTML a] -> UI HTML a
elWithNamespace mNamespace e attrs children = do
  attrs' <- UI.liftNonBlockingSTM $ traverse toAttr attrs
  UI.mapView (pure . VNode e (M.fromList $ fmap fst attrs') mNamespace) (UI.orr (children <> concatMap snd attrs'))

  where
    toAttr :: Props a -> STM ((T.Text, Attr), [UI HTML a])
    toAttr (Props k (PropText v)) = pure ((k, AText v), [])
    toAttr (Props k (PropBool v)) = pure ((k, ABool v), [])
    toAttr (Props k (PropEvent extract)) = do
      n <- newEmptyTMVar
      pure ((k, AEvent (atomically . putTMVar n)), [extract <$> UI.liftSTM (takeTMVar n)])
    toAttr (Props k (PropMap m)) = do
      m' <- mapM toAttr m
      pure ((k, AMap $ M.fromList $ fmap fst m'), concatMap snd m')

text :: T.Text -> UI HTML a
text txt = UI.view [VText txt]

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
div :: [Props a] -> [UI HTML a] -> UI HTML a
div  = el "div"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table :: [Props a] -> [UI HTML a] -> UI HTML a
table  = el "table"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead :: [Props a] -> [UI HTML a] -> UI HTML a
thead  = el "thead"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody :: [Props a] -> [UI HTML a] -> UI HTML a
tbody  = el "tbody"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr :: [Props a] -> [UI HTML a] -> UI HTML a
tr  = el "tr"

-- | Contains `Key`, inteded to be used for child replacement patch
--
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr>

trKeyed :: T.Text -> [Props a] -> [UI HTML a] -> UI HTML a
trKeyed k props = el "tr" (key k:props)

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th :: [Props a] -> [UI HTML a] -> UI HTML a
th  = el "th"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td :: [Props a] -> [UI HTML a] -> UI HTML a
td  = el "td"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot :: [Props a] -> [UI HTML a] -> UI HTML a
tfoot  = el "tfoot"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section :: [Props a] -> [UI HTML a] -> UI HTML a
section  = el "section"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header :: [Props a] -> [UI HTML a] -> UI HTML a
header  = el "header"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer :: [Props a] -> [UI HTML a] -> UI HTML a
footer  = el "footer"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
button :: [Props a] -> [UI HTML a] -> UI HTML a
button = el "button"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form :: [Props a] -> [UI HTML a] -> UI HTML a
form = el "form"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p :: [Props a] -> [UI HTML a] -> UI HTML a
p = el "p"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s :: [Props a] -> [UI HTML a] -> UI HTML a
s = el "s"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul :: [Props a] -> [UI HTML a] -> UI HTML a
ul = el "ul"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span :: [Props a] -> [UI HTML a] -> UI HTML a
span = el "span"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong :: [Props a] -> [UI HTML a] -> UI HTML a
strong = el "strong"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li :: [Props a] -> [UI HTML a] -> UI HTML a
li = el "li"

-- | Contains `Key`, inteded to be used for child replacement patch
--
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li>
--
liKeyed :: T.Text -> [Props a] -> [UI HTML a] -> UI HTML a
liKeyed k props = el "li" (key k:props)

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1 :: [Props a] -> [UI HTML a] -> UI HTML a
h1 = el "h1"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2 :: [Props a] -> [UI HTML a] -> UI HTML a
h2 = el "h2"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3 :: [Props a] -> [UI HTML a] -> UI HTML a
h3 = el "h3"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4 :: [Props a] -> [UI HTML a] -> UI HTML a
h4 = el "h4"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5 :: [Props a] -> [UI HTML a] -> UI HTML a
h5 = el "h5"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6 :: [Props a] -> [UI HTML a] -> UI HTML a
h6 = el "h6"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr :: [Props a] -> UI HTML a
hr = flip (el "hr") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
pre :: [Props a] -> [UI HTML a] -> UI HTML a
pre = el "pre"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input :: [Props a] -> UI HTML a
input = flip (el "input") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label :: [Props a] -> [UI HTML a] -> UI HTML a
label = el "label"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a :: [Props a] -> [UI HTML a] -> UI HTML a
a = el "a"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark :: [Props a] -> [UI HTML a] -> UI HTML a
mark = el "mark"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby :: [Props a] -> [UI HTML a] -> UI HTML a
ruby = el "ruby"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt :: [Props a] -> [UI HTML a] -> UI HTML a
rt = el "rt"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp :: [Props a] -> [UI HTML a] -> UI HTML a
rp = el "rp"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi :: [Props a] -> [UI HTML a] -> UI HTML a
bdi = el "bdi"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo :: [Props a] -> [UI HTML a] -> UI HTML a
bdo = el "bdo"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr :: [Props a] -> UI HTML a
wbr = flip (el "wbr") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
details :: [Props a] -> [UI HTML a] -> UI HTML a
details = el "details"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary :: [Props a] -> [UI HTML a] -> UI HTML a
summary = el "summary"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem :: [Props a] -> [UI HTML a] -> UI HTML a
menuitem = el "menuitem"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu :: [Props a] -> [UI HTML a] -> UI HTML a
menu = el "menu"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset :: [Props a] -> [UI HTML a] -> UI HTML a
fieldset = el "fieldset"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend :: [Props a] -> [UI HTML a] -> UI HTML a
legend = el "legend"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist :: [Props a] -> [UI HTML a] -> UI HTML a
datalist = el "datalist"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup :: [Props a] -> [UI HTML a] -> UI HTML a
optgroup = el "optgroup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen :: [Props a] -> [UI HTML a] -> UI HTML a
keygen = el "keygen"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output :: [Props a] -> [UI HTML a] -> UI HTML a
output = el "output"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress :: [Props a] -> [UI HTML a] -> UI HTML a
progress = el "progress"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter :: [Props a] -> [UI HTML a] -> UI HTML a
meter = el "meter"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center :: [Props a] -> [UI HTML a] -> UI HTML a
center = el "center"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio :: [Props a] -> [UI HTML a] -> UI HTML a
audio = el "audio"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video :: [Props a] -> [UI HTML a] -> UI HTML a
video = el "video"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source :: [Props a] -> UI HTML a
source = flip (el "source") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
track :: [Props a] -> UI HTML a
track = flip (el "track") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embed :: [Props a] -> UI HTML a
embed = flip (el "embed") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
object :: [Props a] -> [UI HTML a] -> UI HTML a
object = el "object"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param :: [Props a] -> UI HTML a
param = flip (el "param") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins :: [Props a] -> [UI HTML a] -> UI HTML a
ins = el "ins"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del :: [Props a] -> [UI HTML a] -> UI HTML a
del = el "del"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small :: [Props a] -> [UI HTML a] -> UI HTML a
small = el "small"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite :: [Props a] -> [UI HTML a] -> UI HTML a
cite = el "cite"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn :: [Props a] -> [UI HTML a] -> UI HTML a
dfn = el "dfn"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr :: [Props a] -> [UI HTML a] -> UI HTML a
abbr = el "abbr"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time :: [Props a] -> [UI HTML a] -> UI HTML a
time = el "time"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var :: [Props a] -> [UI HTML a] -> UI HTML a
var = el "var"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp :: [Props a] -> [UI HTML a] -> UI HTML a
samp = el "samp"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd :: [Props a] -> [UI HTML a] -> UI HTML a
kbd = el "kbd"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption :: [Props a] -> [UI HTML a] -> UI HTML a
caption = el "caption"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup :: [Props a] -> [UI HTML a] -> UI HTML a
colgroup = el "colgroup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col :: [Props a] -> UI HTML a
col = flip (el "col") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
nav :: [Props a] -> [UI HTML a] -> UI HTML a
nav = el "nav"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
article :: [Props a] -> [UI HTML a] -> UI HTML a
article = el "article"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside :: [Props a] -> [UI HTML a] -> UI HTML a
aside = el "aside"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address :: [Props a] -> [UI HTML a] -> UI HTML a
address = el "address"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main_ :: [Props a] -> [UI HTML a] -> UI HTML a
main_ = el "main"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body :: [Props a] -> [UI HTML a] -> UI HTML a
body = el "body"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure :: [Props a] -> [UI HTML a] -> UI HTML a
figure = el "figure"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption :: [Props a] -> [UI HTML a] -> UI HTML a
figcaption = el "figcaption"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl :: [Props a] -> [UI HTML a] -> UI HTML a
dl = el "dl"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt :: [Props a] -> [UI HTML a] -> UI HTML a
dt = el "dt"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd :: [Props a] -> [UI HTML a] -> UI HTML a
dd = el "dd"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img :: [Props a] -> UI HTML a
img = flip (el "img") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe :: [Props a] -> [UI HTML a] -> UI HTML a
iframe = el "iframe"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas :: [Props a] -> [UI HTML a] -> UI HTML a
canvas = el "canvas"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math :: [Props a] -> [UI HTML a] -> UI HTML a
math = el "math"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select :: [Props a] -> [UI HTML a] -> UI HTML a
select = el "select"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option :: [Props a] -> [UI HTML a] -> UI HTML a
option = el "option"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea :: [Props a] -> [UI HTML a] -> UI HTML a
textarea = el "textarea"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub :: [Props a] -> [UI HTML a] -> UI HTML a
sub = el "sub"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup :: [Props a] -> [UI HTML a] -> UI HTML a
sup = el "sup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br :: [Props a] -> UI HTML a
br = flip (el "br") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
ol :: [Props a] -> [UI HTML a] -> UI HTML a
ol = el "ol"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquote :: [Props a] -> [UI HTML a] -> UI HTML a
blockquote = el "blockquote"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
code :: [Props a] -> [UI HTML a] -> UI HTML a
code = el "code"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em :: [Props a] -> [UI HTML a] -> UI HTML a
em = el "em"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i :: [Props a] -> [UI HTML a] -> UI HTML a
i = el "i"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b :: [Props a] -> [UI HTML a] -> UI HTML a
b = el "b"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u :: [Props a] -> [UI HTML a] -> UI HTML a
u = el "u"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q :: [Props a] -> [UI HTML a] -> UI HTML a
q = el "q"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
script :: [Props a] -> [UI HTML a] -> UI HTML a
script = el "script"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
link :: [Props a] -> UI HTML a
link = flip (el "link") []
