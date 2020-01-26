{-# LANGUAGE OverloadedStrings #-}

module Concur.Replica.SVGProps where

import qualified Data.Text as T
import Concur.Replica.Props

version :: T.Text -> Props a
version = textProp "version"

fill :: T.Text -> Props a
fill = textProp "fill"

x :: T.Text -> Props a
x = textProp "x"

y :: T.Text -> Props a
y = textProp "y"

xmlns :: Props a
xmlns  = textProp "xmlns" "http://www.w3.org/2000/svg"
