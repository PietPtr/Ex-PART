import Clash.Prelude
import Definitions

merger :: () -> (Maybe Value, Maybe Value) -> ((), (Value))
merger () (vo, ve) = ((), (res))
    where
        res = case vo of
            Just v -> v
            Nothing -> case ve of
                Just v -> v
                Nothing -> 0

mergerM :: HiddenClockResetEnable dom =>
    Signal dom (Maybe Value, Maybe Value) -> Signal dom (Value)
mergerM = mealy merger ()

{-# ANN topEntity
  (Synthesize
    { t_name = "merger"
    , t_inputs = [ PortName "clk", PortName "rst", PortName "en", PortProduct "" [PortName "vo", PortName "ve"] ]
    , t_output = PortName "res"
    }) #-}

topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Maybe Value, Maybe Value)
    -> Signal System (Value)
topEntity = exposeClockResetEnable mergerM