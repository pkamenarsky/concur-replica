{-# LANGUAGE OverloadedStrings #-}

module Concur.Replica.SVGProps where

import qualified Data.Text as T
import Concur.Replica.Props

version :: T.Text -> Props a
version = textProp "version"

xProp :: T.Text -> Props a
xProp = textProp "x"

yProp :: T.Text -> Props a
yProp = textProp "y"

xmlns :: Props a
xmlns  = textProp "xmlns" "http://www.w3.org/2000/svg"
