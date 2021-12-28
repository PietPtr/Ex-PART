{-# LANGUAGE NumericUnderscores #-}
module Definitions where
import Clash.Prelude
import qualified Data.List as L


(>>>) :: Bits a => a -> Int -> a
(>>>) = shiftR

(<<<) :: Bits a => a -> Int -> a
(<<<) = shiftL

type Value = Unsigned 16

