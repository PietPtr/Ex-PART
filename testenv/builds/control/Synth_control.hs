import Clash.Prelude
import Definitions

control :: (Value) -> (Value, Maybe Value) -> ((Value), (Value))
control (last_val) (next_val, set_val) = ((last_val'), (result_value))
    where
        last_val' = case set_val of
            Just new_value -> new_value
            Nothing -> next_val
    
        result_value = last_val

controlM :: HiddenClockResetEnable dom =>
    Signal dom (Value, Maybe Value) -> Signal dom (Value)
controlM = mealy control (0)

{-# ANN topEntity
  (Synthesize
    { t_name = "control"
    , t_inputs = [ PortName "clk", PortName "rst", PortName "en", PortProduct "" [PortName "next_val", PortName "set_val"] ]
    , t_output = PortName "result_value"
    }) #-}

topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Value, Maybe Value)
    -> Signal System (Value)
topEntity = exposeClockResetEnable controlM