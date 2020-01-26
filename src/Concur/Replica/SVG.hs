{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Concur.Replica.SVG where

import           Concur.Replica.Props     (Props(Props), Prop(PropText, PropBool, PropEvent, PropMap), key)

import qualified Data.Text                as T

import           Replica.VDOM             (Attr(AText, ABool, AEvent, AMap), HTML, DOMEvent, VDOM(VNode, VText))

import Concur.Replica.DOM (WidgetConstraints, elWithNamespace)

import Replica.VDOM.Types (Namespace(Namespace))

el :: forall m a. WidgetConstraints m => T.Text -> [Props a] -> [m a] -> m a
el  = elWithNamespace (Just (Namespace "http://www.w3.org/2000/svg"))

svg :: WidgetConstraints m => [Props a] -> [m a] -> m a
svg  = el "svg"

rect :: WidgetConstraints m => [Props a] -> [m a] -> m a
rect  = el "rect"
