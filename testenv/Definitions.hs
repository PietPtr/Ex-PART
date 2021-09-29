module Definitions where
import Clash.Prelude

type Value = Unsigned 16


(>>>) :: Bits a => a -> Int -> a
(>>>) = shiftR

(<<<) :: Bits a => a -> Int -> a
(<<<) = shiftL
