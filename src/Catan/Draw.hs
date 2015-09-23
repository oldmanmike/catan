-------------------------------------------------------------------------------
-- |
-- Copyright    : (C) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : GHC
--
-------------------------------------------------------------------------------
module Catan.Draw (draw) where

import Catan.Types

draw:: [DevCard] -> (DevCard,[DevCard])
draw (x:xs) = (x,xs)
