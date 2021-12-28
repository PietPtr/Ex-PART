import Clash.Prelude
import Definitions
import Debug.Trace
import qualified Data.List as L
import Data.Maybe



-- Monolithic file, everything is inlined (by default)

control :: (Value) -> (Value, Maybe Value) -> ((Value), (Value))
control (last_val) (next_val, set_val) = ((last_val'), (result_value))
    where
        last_val' = case set_val of
            Just new_value -> new_value
            Nothing -> next_val
    
        result_value = last_val

merger :: () -> (Maybe Value, Maybe Value) -> ((), (Value))
merger () (vo, ve) = ((), (res))
    where
        res = case vo of
            Just v -> v
            Nothing -> case ve of
                Just v -> v
                Nothing -> 0

onOdd :: () -> (Maybe Value) -> ((), (Maybe Value))
onOdd () (val) = ((), (res))
    where
        res = case val of
            Just v -> Just $ (v <<< 1 + v) + 1
            Nothing -> Nothing

onEven :: () -> (Maybe Value) -> ((), (Maybe Value))
onEven () (val) = ((), (res))
    where
        res = case val of
            Just v -> Just $ v >>> 1
            Nothing -> Nothing

router :: () -> (Value) -> ((), (Maybe Value, Maybe Value))
router () (val) = ((), (odd, even))
    where
        even = if testBit val 0 then Nothing else Just val
        odd  = if testBit val 0 then Just val else Nothing

controlM :: HiddenClockResetEnable dom =>
    Signal dom (Value, Maybe Value) -> Signal dom (Value)
controlM = mealy control (0)

mergerM :: HiddenClockResetEnable dom =>
    Signal dom (Maybe Value, Maybe Value) -> Signal dom (Value)
mergerM = mealy merger ()

onOddM :: HiddenClockResetEnable dom =>
    Signal dom (Maybe Value) -> Signal dom (Maybe Value)
onOddM = mealy onOdd ()

onEvenM :: HiddenClockResetEnable dom =>
    Signal dom (Maybe Value) -> Signal dom (Maybe Value)
onEvenM = mealy onEven ()

routerM :: HiddenClockResetEnable dom =>
    Signal dom (Value) -> Signal dom (Maybe Value, Maybe Value)
routerM = mealy router ()


system :: HiddenClockResetEnable dom =>
    Signal dom (Maybe Value) -> Signal dom (Value)
system input = (controller_result_value)
    where
        (this_setting) =  input
        (controller_result_value) = controlM $ bundle (collatzer_val_out, this_setting)
        (collatzer_val_out) = collatzer (controller_result_value)
        collatzer :: HiddenClockResetEnable dom =>
            Signal dom (Value) -> Signal dom (Value)
        collatzer input = (merger_res)
            where
                (this_val_in) =  input
                (merger_res) = mergerM $ bundle (onOdd_res, onEven_res)
                (onEven_res) = onEvenM (router_even)
                (onOdd_res) = onOddM (router_odd)
                (router_odd, router_even) = unbundle $ routerM (this_val_in)


{-# ANN topEntity
  (Synthesize
    { t_name = "system"
    , t_inputs = [ PortName "clk", PortName "rst", PortName "en", PortName "setting" ]
    , t_output = PortName "result"
    }) #-}

topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Maybe Value)
    -> Signal System (Value)
topEntity = exposeClockResetEnable system