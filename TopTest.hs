import Types
import Parser
import Generator
import JSONBuilder
import Yosys
import Nextpnr

import Data.Either
import Data.Aeson
import Data.Text (pack, unpack)
import qualified Data.Map as Map
import Control.Monad
import Control.Concurrent
import System.Directory

expc :: IO Program
expc = (fromRight (Program [] [] [])) <$> parse_expc "examples/collatz.expc"

expc' :: Program
expc' = Program ["type Value = Unsigned 8"] [Combinatory "\n    (>>>) :: Bits a => a -> Int -> a\n    (>>>) = shiftR\n    \n    (<<<) :: Bits a => a -> Int -> a\n    (<<<) = shiftL\n"] [Component "control" [] [SInput "next_val" "Value",SInput "set_val" "Maybe Value",SState "last_val" (Constant 0) "Value",SOutput "result_value" "Value"] "last_val' = case set_val of\n        Just new_value -> new_value\n        Nothing -> next_val\n\n    result_value = last_val\n",Component "merger" [] [SInput "vo" "Maybe Value",SInput "ve" "Maybe Value",SOutput "res" "Value"] "res = case vo of\n        Just v -> v\n        Nothing -> case ve of\n            Just v -> v\n            Nothing -> 0\n",Component "onOdd" [] [SInput "val" "Maybe Value",SOutput "res" "Maybe Value"] "res = case value of\n        Just v -> Just $ (v <<< 1 + v) + 1\n        Nothing -> Nothing\n",Component "onEven" [] [SInput "val" "Maybe Value",SOutput "res" "Maybe Value"] "res = case value of\n        Just v -> Just $ v >>> 1\n        Nothing -> Nothing\n",Component "router" [] [SInput "val" "Value",SOutput "odd" "Maybe Value",SOutput "even" "Maybe Value"] "odd  = if testBit val 0 then Nothing else Just val\n    even = if testBit val 0 then Just val else Nothing\n"]

expi :: IO System
expi = (fromRight emptySystem) <$> parse_expi "examples/collatz.expi"

expi' :: System
expi' = System {sys_flattened = False, sys_id = "system", sys_size = (6,6), sys_coords = (CConst 2,CConst 2), sys_iodefs = [Output "result" "Value",Input "setting" "Maybe Value"], sys_instances = [Instance {ins_name = "controller", ins_cmp = "control", ins_args = [], ins_size = (6,2), ins_coords = (CConst 0,CConst 0)}], sys_connections = [Connection (CID "controller" "result_value") (CID "this" "result"),Connection (CID "this" "setting") (CID "controller" "set_val"),Connection (CID "collatzer" "val_out") (CID "controller" "next_val"),Connection (CID "controller" "result_value") (CID "collatzer" "val_in")], sys_repetitions = [], sys_subsystems = [System {sys_flattened = False, sys_id = "collatzer", sys_size = (6,4), sys_coords = (CConst 0,CHeight "controller"), sys_iodefs = [Output "val_out" "Value",Input "val_in" "Value"], sys_instances = [Instance {ins_name = "merger", ins_cmp = "merger", ins_args = [], ins_size = (1,4), ins_coords = (CAdd (CX "onOdd") (CWidth "onOdd"),CConst 0)},Instance {ins_name = "onEven", ins_cmp = "onEven", ins_args = [], ins_size = (4,2), ins_coords = (CWidth "router",CHeight "onOdd")},Instance {ins_name = "onOdd", ins_cmp = "onOdd", ins_args = [], ins_size = (4,2), ins_coords = (CWidth "router",CConst 0)},Instance {ins_name = "router", ins_cmp = "router", ins_args = [], ins_size = (1,4), ins_coords = (CConst 0,CConst 0)}], sys_connections = [Connection (CID "merger" "res") (CID "this" "val_out"),Connection (CID "onEven" "res") (CID "merger" "ve"),Connection (CID "onOdd" "res") (CID "merger" "vo"),Connection (CID "router" "even") (CID "onEven" "val"),Connection (CID "router" "odd") (CID "onOdd" "val"),Connection (CID "this" "val_in") (CID "router" "val")], sys_repetitions = [], sys_subsystems = []}]}

-- TODO: maak een main die dit goed aan elkaar knoopt

-- helper dingetjes voor ghci
comps = case expc' of
    (Program _ _ comps) -> comps
insts = sys_instances $ head $ sys_subsystems expi'
conns = sys_connections $ head $ sys_subsystems expi'

flow :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
flow expcName expiName lpf outDir = do
    startDir <- getCurrentDirectory
    lpfLoc <- makeAbsolute lpf
    parsed <- parse_expc expcName
    case parsed of
        (Left error) -> putStrLn $ "[Ex-PART] Parse error: " ++ show error
        (Right result) -> putStrLn "[Ex-PART] Succesfully parsed .expc"
    guard (isRight parsed)
    expc <- pure $ fromRight undefined parsed

    parsed <- parse_expi expiName
    case parsed of
        (Left error) -> putStrLn $ "[Ex-PART] Parse error: " ++ show error
        (Right result) -> putStrLn "[Ex-PART] Succesfully parsed .expi"
    guard (isRight parsed)
    expi <- pure $ fromRight undefined parsed

    putStrLn $ "[Ex-PART] Creating directory " ++ outDir ++ "..."
    createDirectoryIfMissing True outDir
    threadDelay 1000 -- TODO: there is a dependence on these statements, but they're executed concurrently...
    setCurrentDirectory outDir

    putStrLn "[Ex-PART] Generating Clash code..."
    generateClash expc

    putStrLn "[Ex-PART] Generating locations.json..."
    writeLocationsJSON expi

    putStrLn "[Ex-PART] Compiling Clash code to Verilog..."
    compileToVerilog expc

    putStrLn "[Ex-PART] Grouping Verilog files into one file..."
    groupVerilogs expc

    putStrLn "[Ex-PART] Synthesizing components to JSON..."
    synthesizeTop

    putStrLn "[Ex-PART] Connecting synthesized JSON according to expi file..."
    customConnect expc expi

    putStrLn "[Ex-PART] Combining JSONs..."
    combineJSONs outDir

    putStrLn "[Ex-PART] Performing place and route using expi constraints..."
    nextpnr lpfLoc

    setCurrentDirectory startDir
    putStrLn $ "[Ex-PART] Done. Bitstream is " ++ outDir ++ "/bitstream.config "


make = flow "examples/collatz.expc" "examples/collatz.expi" "examples/collatz.lpf" "testenv"