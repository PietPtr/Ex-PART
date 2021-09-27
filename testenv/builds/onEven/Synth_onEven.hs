import Clash.Prelude
import Definitions

onEven :: () -> (Maybe Value) -> ((), (Maybe Value))
onEven () (value) = ((), (res))
    where
        res = case value of
            Just v -> Just $ v >>> 1
            Nothing -> Nothing

onEvenM :: HiddenClockResetEnable dom =>
    Signal dom (Maybe Value) -> Signal dom (Maybe Value)
onEvenM = mealy onEven ()

{-# ANN topEntity
  (Synthesize
    { t_name = "onEven"
    , t_inputs = [ PortName "clk", PortName "rst", PortName "en", PortName "value" ]
    , t_output = PortName "res"
    }) #-}

topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Maybe Value)
    -> Signal System (Maybe Value)
topEntity = exposeClockResetEnable onEvenM