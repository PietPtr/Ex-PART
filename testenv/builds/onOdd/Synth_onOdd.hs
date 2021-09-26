import Clash.Prelude
import Definitions

onOdd :: () -> (Maybe Value) -> ((), (Maybe Value))
onOdd () (val) = ((), (res))
    where
        res = case value of
            Just v -> Just $ (v <<< 1 + v) + 1
            Nothing -> Nothing

onOddM :: HiddenClockResetEnable dom =>
    Signal dom (Maybe Value) -> Signal dom (Maybe Value)
onOddM = mealy onOdd ()

{-# ANN topEntity
  (Synthesize
    { t_name = "onOdd"
    , t_inputs = [ PortName "clk", PortName "rst", PortName "en", PortProduct "" [PortName "val"] ]
    , t_output = PortProduct "" [PortName "res"]
    }) #-}

topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Maybe Value)
    -> Signal System (Maybe Value)
topEntity = exposeClockResetEnable onOddM