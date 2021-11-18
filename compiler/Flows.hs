module Flows where

import Types
import Parser
import Unroll
import Generator
import JSONBuilder
import Yosys
import Nextpnr

import Data.Either
import Data.Aeson
import Data.Text (pack, unpack)
import Data.List
import qualified Data.Map as Map
import Control.Monad
import Control.Concurrent
import System.Directory
import Data.List.Split

-- All compile flows in this file assume that they are in the correct output directory
-- except for clean, which creates the directory.
clean :: Program -> System -> FilePath -> FilePath -> FilePath -> IO ()
clean expc_rolled expi_rolled expcPath expiPath outDir = do
    putStrLn $ "[Ex-PART] Creating directory `" ++ outDir ++ "`..."
    createDirectoryIfMissing True outDir -- TODO: maak dit misschien een functie zodat de drie statements niet gekopieerd hoeven?
    threadDelay 1000 -- TODO: there is a dependence on these statements, but they're executed concurrently...
    setCurrentDirectory outDir

    putStrLn $ "[Ex-PART] Unrolling repeat & chain statements..."
    (expc, expi) <- pure $ unroll expc_rolled expi_rolled

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

    putStrLn "[Ex-PART] Connecting synthesized JSON according to expi file..."
    customConnect expc expi


expcChanged :: Program -> System -> [Component] -> [Component] -> [Component] -> IO ()
expcChanged expc_rolled expi_rolled changed deleted newcmps = do
    expc_current <- pure $ expc_rolled {prg_cmps=(changed ++ newcmps)}
    putStrLn "[Ex-PART] Unrolling repeat & chain statements..."
    (expc, expi) <- pure $ unroll expc_rolled expi_rolled

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

    putStrLn "[Ex-PART] Connecting synthesized JSON according to expi file..."
    customConnect expc expi


expiChanged :: Program -> System -> IO ()
expiChanged expc_rolled expi_rolled = do
    putStrLn $ "[Ex-PART] Unrolling repeat & chain statements..."
    (expc, expi) <- pure $ unroll expc_rolled expi_rolled

    putStrLn "[Ex-PART] Generating locations.json..."
    writeLocationsJSON expi

    putStrLn $ "[Ex-PART] Flattening design for Clash simulation..."
    flatten expc expi

    putStrLn "[Ex-PART] Connecting synthesized JSON according to expi file..."
    customConnect expc expi


monolithic :: Program -> System -> FilePath -> FilePath -> IO ()
monolithic expc_rolled expi_rolled lpfPath outDir = do
    let outDir' = (slashscrape outDir) ++ "_monolithic"

    putStrLn $ "[Ex-PART] Creating directory `" ++ outDir' ++ "`..."
    createDirectoryIfMissing True outDir'
    threadDelay 1000
    setCurrentDirectory outDir'

    putStrLn $ "[Ex-PART] Unrolling repeat & chain statements..."
    (expc, expi) <- pure $ unroll expc_rolled expi_rolled

    putStrLn "[Ex-PART] Generating Clash code..."
    generateClash expc

    putStrLn $ "[Ex-PART] Flattening design for Clash simulation..."
    flatten expc expi

    putStrLn "[Ex-PART] Compiling full design to Verilog..."
    compileFullToVerilog

    putStrLn "[Ex-PART] Synthesizing full design..."
    synthesizeMonolithic

    putStrLn "[Ex-PART] Performing place and route without expi constraints..."
    nextpnr lpfPath []

    where
        slashscrape "" = ""
        slashscrape p = if last p == '/' then init (slashscrape p) else p

-- Compile flow that synthesizes/place & routes everything of all components separately s.t.
-- resource usage of individual components can be analyzed
-- Can be used as clean build, and after folder has been made.
resource :: Program -> FilePath -> FilePath -> IO ()
resource expc lpfPath outDir = do
    lpfLoc <- makeAbsolute lpfPath
    putStrLn $ "[Ex-PART] Creating directory `" ++ outDir ++ "`..."
    createDirectoryIfMissing True outDir
    threadDelay 1000
    setCurrentDirectory outDir


    -- buildsExist <- doesPathExist "builds"
    -- if not buildsExist then do
    putStrLn "[Ex-PART] Generating Clash code..."
    generateClash expc

    putStrLn "[Ex-PART] Compiling Clash code to Verilog..."
    compileToVerilog expc
    -- else do
    --     putStrLn "[Ex-PART] `builds` folder exists, skipping Clash compilation (run clean compile manually if outdated)"


    setCurrentDirectory "builds/"
    components' <- listDirectory "."
    let components = filter (\path -> not $ isPrefixOf "." path) components'
    
    -- synthesize every component in builds/
    putStrLn "[Ex-PART] Synthesizing component..."
    mapM_ synthesizeIndividual components

    -- place and route every component in builds/
    putStrLn "[Ex-PART] Placing and routing component..."
    mapM_ (pnrOne lpfLoc) components

    where
        runToolOn cmpName tool = do
            putStrLn $ "        | ..." ++ cmpName
            setCurrentDirectory cmpName
            tool
            setCurrentDirectory ".."

        pnrOne lpfLoc cmpName = runToolOn cmpName $
            nextpnr lpfLoc ["--out-of-context"]
            
        
        synthesizeIndividual cmpName = runToolOn cmpName $
            runYosys ["-p", 
                "read_verilog hdl/Main.topEntity/" ++ cmpName ++ ".v; " 
                ++ synth_ecp5 ++ " -json synthesized.json"]