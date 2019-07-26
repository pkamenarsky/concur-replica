{-# LANGUAGE OverloadedStrings #-}

module Concur.Replica.Events where

import           Concur.Replica.Props     (Props, mkProp)

import           Data.Aeson               ((.=), (.:), (.:?))
import qualified Data.Aeson               as A

import qualified Data.Text                as T

import           Replica.VDOM.Types       (DOMEvent(getDOMEvent), Attr'(AEvent))

-- TODO: return Maybe here
extractResult :: A.Result a -> a
extractResult (A.Success a) = a
extractResult (A.Error e)   = error e

data Target = Target
  { targetValue :: T.Text
  }

instance A.FromJSON Target where
  parseJSON (A.Object o) = Target
    <$> o .: "value"
  parseJSON _ = fail "Expected object"

data BaseEvent = BaseEvent
  { bubbles          :: !Bool
  , cancelable       :: !Bool
  , composed         :: !Bool
  , currentTarget    :: !Target
  , defaultPrevented :: !Bool
  , eventPhase       :: !Int
  , target           :: !Target
  , timeStamp        :: !Double
  , eventType        :: !T.Text
  , isTrusted        :: !Bool
  }

instance A.FromJSON BaseEvent where
  parseJSON (A.Object o) = BaseEvent
    <$> o .: "bubbles"
    <*> o .: "cancelable"
    <*> o .: "composed"
    <*> o .: "currentTarget"
    <*> o .: "defaultPrevented"
    <*> o .: "eventPhase"
    <*> o .: "target"
    <*> o .: "timeStamp"
    <*> o .: "type"
    <*> o .: "isTrusted"
  parseJSON _ = fail "Expected object"

data MouseEvent = MouseEvent
  { mouseBaseEvent     :: !BaseEvent
  , mouseAltKey        :: !Bool
  , mouseButton        :: !Int
  , mouseButtons       :: !Int
  , mouseClientX       :: !Int
  , mouseClientY       :: !Int
  , mouseCtrlKey       :: !Bool
  , mouseMetaKey       :: !Bool
  , mouseMovementX     :: !Int
  , mouseMovementY     :: !Int
  , mouseRegion        :: !(Maybe T.Text)
  , mouseRelatedTarget :: !(Maybe Target)
  , mouseScreenX       :: !Int
  , mouseScreenY       :: !Int
  , mouseShiftKey      :: !Bool
  }

instance A.FromJSON MouseEvent where
  parseJSON obj@(A.Object o) = MouseEvent
    <$> A.parseJSON obj
    <*> o .:  "altKey"
    <*> o .:  "button"
    <*> o .:  "buttons"
    <*> o .:  "clientX"
    <*> o .:  "clientY"
    <*> o .:  "ctrlKey"
    <*> o .:  "metaKey"
    <*> o .:  "movementX"
    <*> o .:  "movementY"
    <*> o .:? "region"
    <*> o .:? "relatedTarget"
    <*> o .:  "screenX"
    <*> o .:  "screenY"
    <*> o .:  "shiftKey"
  parseJSON _ = fail "Expected object"

data KeyboardEvent = KeyboardEvent
  { kbdBaseEvent   :: !BaseEvent
  , kbdAltKey      :: !Bool
  , kbdCtrlKey     :: !Bool
  , kbdIsComposing :: !Bool
  , kbdKey         :: !T.Text
  , kbdLocation    :: !Int
  , kbdMetaKey     :: !Bool
  , kbdRepeat      :: !Bool
  , kbdShiftKey    :: !Bool
  }

instance A.FromJSON KeyboardEvent where
  parseJSON obj@(A.Object o) = KeyboardEvent
    <$> A.parseJSON obj
    <*> o .: "altKey"
    <*> o .: "ctrlKey"
    <*> o .: "isComposing"
    <*> o .: "key"
    <*> o .: "location"
    <*> o .: "metaKey"
    <*> o .: "repeat"
    <*> o .: "shiftKey"
  parseJSON _ = fail "Expected object"

--------------------------------------------------------------------------------

-- | `blur` event defined with custom options
--
-- <https://developer.mozilla.org/en-US/docs/Web/Events/blur>
--
onBlur :: Props BaseEvent
onBlur = mkProp "onBlur" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClick :: Props MouseEvent
onClick = mkProp "onClick" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/focus
onFocus :: Props BaseEvent
onFocus = mkProp "onFocus" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dblclick
onDoubleClick :: Props MouseEvent
onDoubleClick = mkProp "onDblClick" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/input
onInput :: Props BaseEvent
onInput = mkProp "onInput" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/change
onChange :: Props BaseEvent
onChange = mkProp "onChange" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDown :: Props KeyboardEvent
onKeyDown = mkProp "onKeyDown" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keypress
onKeyPress :: Props KeyboardEvent
onKeyPress = mkProp "onKeyPress" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keyup
onKeyUp :: Props KeyboardEvent
onKeyUp = mkProp "onKeyUp" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseup
onMouseUp :: Props MouseEvent
onMouseUp = mkProp "onMouseUp" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mousedown
onMouseDown :: Props MouseEvent
onMouseDown = mkProp "onMouseDown" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseenter
onMouseEnter :: Props MouseEvent
onMouseEnter = mkProp "onMouseEnter" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseleave
onMouseLeave :: Props MouseEvent
onMouseLeave = mkProp "onMouseLeave" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseover
onMouseOver :: Props MouseEvent
onMouseOver = mkProp "onMouseOver" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseout
onMouseOut :: Props MouseEvent
onMouseOut = mkProp "onMouseOut" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragstart
onDragStart :: Props MouseEvent
onDragStart = mkProp "onDragStart" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragover
onDragOver :: Props MouseEvent
onDragOver = mkProp "onDragOver" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragend
onDragEnd :: Props MouseEvent
onDragEnd = mkProp "onDragEnd" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragenter
onDragEnter :: Props MouseEvent
onDragEnter = mkProp "onDragEnter" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragleave
onDragLeave :: Props MouseEvent
onDragLeave = mkProp "onDragLeave" (AEvent (extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/drag
onDrag :: Props MouseEvent
onDrag = mkProp "onDrag" (AEvent (extractResult . A.fromJSON . getDOMEvent))
