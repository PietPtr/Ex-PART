import Clash.Prelude
import Definitions

router :: () -> (Value) -> ((), (Maybe Value, Maybe Value))
router () (val) = ((), (odd, even))
    where
        odd  = if testBit val 0 then Nothing else Just val
        even = if testBit val 0 then Just val else Nothing

routerM :: HiddenClockResetEnable dom =>
    Signal dom (Value) -> Signal dom (Maybe Value, Maybe Value)
routerM = mealy router ()

{-# ANN topEntity
  (Synthesize
    { t_name = "router"
    , t_inputs = [ PortName "clk", PortName "rst", PortName "en", PortProduct "" [PortName "val"] ]
    , t_output = PortProduct "" [PortName "odd", PortName "even"]
    }) #-}

topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Value)
    -> Signal System (Maybe Value, Maybe Value)
topEntity = exposeClockResetEnable routerM