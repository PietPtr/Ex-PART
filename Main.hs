module Main where

import Types
import Parser
import Generator
import JSONBuilder
import Yosys
import Nextpnr

import Expi

import Data.Either
import Data.Aeson
import Data.Text (pack, unpack)
import qualified Data.Map as Map
import Control.Monad
import Control.Concurrent
import System.Directory

expc :: IO Program
expc = (fromRight (Program [] [] [])) <$> parse_expc "examples/collatz.expc"

expi :: IO System
expi = do
    prog <- expc
    syst <- parse_expi (prg_cmps prog) "examples/collatz.expi"
    return (fromRight emptySystem syst)

-- helper dingetjes voor ghci
-- comps = case expc' of
--     (Program _ _ comps) -> comps
-- insts = sys_instances $ head $ sys_subsystems expi'
-- conns = sys_connections $ head $ sys_subsystems expi'

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

    parsed <- parse_expi (prg_cmps expc) expiName
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

    putStrLn $ "[Ex-PART] Flattening design for Clash simulation..."
    flatten expc expi

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

main = pure ()