import Types
import Parser
import Generator
import Preprocessing
import Yosys

import Data.Either

expc :: IO Program
expc = (fromRight (Program [] [] [])) <$> parse_expc "examples/collatz.expc"

expc' :: Program
expc' = Program ["type Value = Unsigned 8"] [Combinatory "\n    (>>>) :: Bits a => a -> Int -> a\n    (>>>) = shiftR\n    \n    (<<<) :: Bits a => a -> Int -> a\n    (<<<) = shiftL\n"] [Component "control" [] [SInput "next_val" "Value",SInput "set_val" "Maybe Value",SState "last_val" (Constant 0) "Value",SOutput "result_value" "Value"] "last_val' = case set_val of\n        Just new_value -> new_value\n        Nothing -> next_val\n\n    result_value = last_val\n",Component "merger" [] [SInput "vo" "Maybe Value",SInput "ve" "Maybe Value",SOutput "res" "Value"] "res = case vo of\n        Just v -> v\n        Nothing -> case ve of\n            Just v -> v\n            Nothing -> 0\n",Component "onOdd" [] [SInput "val" "Maybe Value",SOutput "res" "Maybe Value"] "res = case value of\n        Just v -> Just $ (v <<< 1 + v) + 1\n        Nothing -> Nothing\n",Component "onEven" [] [SInput "val" "Maybe Value",SOutput "res" "Maybe Value"] "res = case value of\n        Just v -> Just $ v >>> 1\n        Nothing -> Nothing\n",Component "router" [] [SInput "val" "Value",SOutput "odd" "Maybe Value",SOutput "even" "Maybe Value"] "odd  = if testBit val 0 then Nothing else Just val\n    even = if testBit val 0 then Just val else Nothing\n"]

expi :: IO System
expi = (fromRight emptySystem) <$> parse_expi "examples/collatz.expi"

expi' :: System
expi' = System {sys_flattened = False, sys_id = "system", sys_size = (6,6), sys_coords = (CConst 2,CConst 2), sys_iodefs = [Output "result" "Value",Input "setting" "Maybe Value"], sys_instances = [Instance "controller" "control" [] (6,2) (CConst 0,CConst 0)], sys_connections = [Connection (CID "controller" "result_value") (CID "this" "result"),Connection (CID "this" "setting") (CID "controller" "set_val"),Connection (CID "collatzer" "output") (CID "controller" "next_val"),Connection (CID "controller" "result_value") (CID "collatzer" "input")], sys_repetitions = [], sys_subsystems = [System {sys_flattened = False, sys_id = "collatzer", sys_size = (6,4), sys_coords = (CConst 0,CHeight "controller"), sys_iodefs = [Output "val_out" "Value",Input "val_in" "Value"], sys_instances = [Instance "merger" "merger" [] (1,4) (CAdd (CX "onOdd") (CWidth "onOdd"),CConst 0),Instance "onEven" "onEven" [] (4,2) (CWidth "router",CHeight "onOdd"),Instance "onOdd" "onOdd" [] (4,2) (CWidth "router",CConst 0),Instance "router" "router" [] (1,4) (CConst 0,CConst 0)], sys_connections = [Connection (CID "merger" "res") (CID "this" "val_out"),Connection (CID "onEven" "res") (CID "merger" "ve"),Connection (CID "onOdd" "res") (CID "merger" "vo"),Connection (CID "router" "even") (CID "onEven" "val"),Connection (CID "router" "odd") (CID "onOdd" "val"),Connection (CID "this" "val_in") (CID "router" "val")], sys_repetitions = [], sys_subsystems = []}]}


step2 = do
    program <- expc
    generateClash "testenv" program

step3 = do
    program <- expc -- natuurlijk gaat dit in het echt niet twee keer parsen
    compileToVerilog "testenv" program

step4 = do
    program <- expc
    groupVerilogs "testenv" program
    synthesizeTop "testenv"

