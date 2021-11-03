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

clean :: Program -> System -> FilePath -> FilePath -> FilePath -> IO ()
clean expc expi_reps expcPath expiPath outDir = do
    putStrLn $ "[Ex-PART] Creating directory `" ++ outDir ++ "`..."
    createDirectory outDir
    threadDelay 1000 -- TODO: there is a dependence on these statements, but they're executed concurrently...

    putStrLn $ "[Ex-PART] Copying source files to build directory..."
    copyFile expcPath (outDir ++ "/build.expc")
    copyFile expiPath (outDir ++ "/build.expi")

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

    -- putStrLn "[Ex-PART] Generating resource usage report..." 
    -- resourceReportJSON expc

    putStrLn "[Ex-PART] Connecting synthesized JSON according to expi file..."
    customConnect expc expi

    putStrLn "[Ex-PART] Combining JSONs..."
    combineJSONs outDir

    pure ()
