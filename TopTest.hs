import Types
import Parser
import Generator
import Yosys
import JSONBuilder
import Postprocessing

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
-- step1 = do
--     program <- expc
--     system <- expi
--     generateClash "testenv" program
--     writeLocationsJSON "testenv" system

-- step2 = do
--     program <- expc -- natuurlijk gaat dit in het echt niet twee keer parsen
--     compileToVerilog "testenv" program

-- step3 = do
--     program <- expc
--     system  <- expi
--     groupVerilogs "testenv" program
--     synthesizeTop "testenv"

-- step4 = do
--     program <- expc
--     system  <- expi
--     customConnect "testenv" program system
--     combineJSONs "testenv"


-- helper dingetjes voor ghci
comps = case expc' of
    (Program _ _ comps) -> comps
insts = sys_instances $ head $ sys_subsystems expi'
conns = sys_connections $ head $ sys_subsystems expi'

flow :: FilePath -> FilePath -> FilePath -> IO ()
flow expcName expiName outDir = do
    startDir <- getCurrentDirectory
    parsed <- parse_expc expcName
    case parsed of
        (Left error) -> putStrLn $ "Parse error: " ++ show error
        (Right result) -> putStrLn "Succesfully parsed .expc"
    guard (isRight parsed)
    expc <- pure $ fromRight undefined parsed

    parsed <- parse_expi expiName
    case parsed of
        (Left error) -> putStrLn $ "Parse error: " ++ show error
        (Right result) -> putStrLn "Succesfully parsed .expi"
    guard (isRight parsed)
    expi <- pure $ fromRight undefined parsed

    createDirectoryIfMissing True outDir
    putStrLn $ "Created directory " ++ outDir
    threadDelay 1000 -- TODO: there is a dependence on these statements, but they're executed concurrently...
    setCurrentDirectory outDir

    generateClash expc
    putStrLn "Generated Clash code."

    writeLocationsJSON expi
    putStrLn "Generated locations.json"

    compileToVerilog expc
    putStrLn "Compiled Clash code to Verilog."

    groupVerilogs expc
    putStrLn "Grouped Verilog files into one large file."

    synthesizeTop
    putStrLn "Synthesized top module to JSON."

    customConnect expc expi
    putStrLn "Connected synthesized JSON according."

    combineJSONs outDir
    putStrLn "Combined JSONs."

    putStrLn $ "Synthesis done, place and route can be performed with []\n    moving back to " ++ startDir ++ "."
    setCurrentDirectory startDir



make = flow "examples/collatz.expc" "examples/collatz.expi" "testenv"