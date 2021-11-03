module Flows where

import Types
import Parser
import Repetition
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
import Data.List.Split

-- All compile flows in this file assume that they are in the correct output directory
-- except for clean, which creates the directory.
clean :: Program -> System -> FilePath -> FilePath -> FilePath -> IO ()
clean expc expi_reps expcPath expiPath outDir = do
    putStrLn $ "[Ex-PART] Creating directory `" ++ outDir ++ "`..."
    createDirectory outDir
    threadDelay 1000 -- TODO: there is a dependence on these statements, but they're executed concurrently...
    setCurrentDirectory outDir

    putStrLn $ "[Ex-PART] Unrolling repeat & chain statements..."
    expi <- pure $ unroll expc expi_reps

    putStrLn "[Ex-PART] Generating locations.json..."
    writeLocationsJSON expi

    putStrLn "[Ex-PART] Generating Clash code..."
    generateClash expc

    putStrLn $ "[Ex-PART] Flattening design for Clash simulation..."
    flatten expc expi

    putStrLn "[Ex-PART] Compiling Clash code to Verilog..."
    compileToVerilog expc

    putStrLn "[Ex-PART] Grouping Verilog files into one file..."
    groupVerilogs expc

    putStrLn "[Ex-PART] Synthesizing components to JSON..."
    synthesizeTop

    putStrLn "[Ex-PART] Generating resource usage report..." 
    resourceReportJSON expc

    putStrLn "[Ex-PART] Connecting synthesized JSON according to expi file..."
    customConnect expc expi



expcChanged :: Program -> System -> [Component] -> [Component] -> [Component] -> IO ()
expcChanged expc expi_reps changed deleted newcmps = do
    expc_current <- pure $ expc {prg_cmps=(changed ++ newcmps)}
    putStrLn "[Ex-PART] Unrolling repeat & chain statements..."
    expi <- pure $ unroll expc expi_reps

    putStrLn "[Ex-PART] Generating locations.json..."
    writeLocationsJSON expi

    putStrLn "[Ex-PART] Generating Clash code..."
    generateClash expc_current

    putStrLn "[Ex-PART] Flattening design for Clash simulation..."
    flatten expc expi

    putStrLn "[Ex-PART] Compiling Clash code of changed components to Verilog..."
    compileToVerilog expc_current

    putStrLn "[Ex-PART] Removing deleted components..."
    mapM_ (\c -> removeDirectoryRecursive $ "builds/" ++ cmp_name c) deleted

    putStrLn "[Ex-PART] Grouping Verilog files into one file..."
    groupVerilogs expc

    putStrLn "[Ex-PART] Synthesizing components to JSON..."
    synthesizeTop

    putStrLn "[Ex-PART] Generating resource usage report..." 
    resourceReportJSON expc

    putStrLn "[Ex-PART] Connecting synthesized JSON according to expi file..."
    customConnect expc expi


expiChanged :: Program -> System -> IO ()
expiChanged expc expi_reps = do
    putStrLn $ "[Ex-PART] Unrolling repeat & chain statements..."
    expi <- pure $ unroll expc expi_reps

    putStrLn "[Ex-PART] Generating locations.json..."
    writeLocationsJSON expi

    putStrLn $ "[Ex-PART] Flattening design for Clash simulation..."
    flatten expc expi

    putStrLn "[Ex-PART] Connecting synthesized JSON according to expi file..."
    customConnect expc expi