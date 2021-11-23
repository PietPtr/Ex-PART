module Steps where

import Types

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

createDirAndEnter :: FilePath -> IO ()
createDirAndEnter outDir = do
    putStrLn $ "[Ex-PART] Creating directory `" ++ outDir ++ "`..."
    createDirectoryIfMissing True outDir
    threadDelay 1000 -- This is terrible (:
    setCurrentDirectory outDir

unrollRepetitions :: Program -> System -> IO (Program, System)
unrollRepetitions expc expi = do
    putStrLn $ "[Ex-PART] Unrolling repeat & chain statements..."
    pure $ unroll expc expi

writeLocations :: System -> IO ()
writeLocations expi = do
    putStrLn "[Ex-PART] Generating locations.json..."
    writeLocationsJSON expi

generateClash :: Program -> IO ()
generateClash expc = do
    putStrLn "[Ex-PART] Generating Clash code..."
    Generator.generateClash expc

flattenForSim :: Program -> System -> IO ()
flattenForSim expc expi = do
    putStrLn $ "[Ex-PART] Flattening design for Clash simulation..."
    flatten expc expi

compileToVerilog :: Program -> IO ()
compileToVerilog expc = do
    putStrLn "[Ex-PART] Compiling Clash code to Verilog..."
    Yosys.compileToVerilog expc

groupVerilogFiles :: Program -> IO ()
groupVerilogFiles expc = do
    putStrLn "[Ex-PART] Grouping Verilog files into one file..."
    groupVerilogs expc

synthesizeComponents :: IO ()
synthesizeComponents = do
    putStrLn "[Ex-PART] Synthesizing components to JSON..."
    synthesizeTop

connectComponents :: Program -> System -> IO ()
connectComponents expc expi = do
    putStrLn "[Ex-PART] Connecting synthesized JSON according to expi file..."
    customConnect expc expi

removeDeleted :: [Component] -> IO ()
removeDeleted deleted = do
    putStrLn "[Ex-PART] Removing deleted components..."
    mapM_ (\c -> removeDirectoryRecursive $ "builds/" ++ cmp_name c) deleted


monolithicToVerilog :: IO ()
monolithicToVerilog = do
    putStrLn "[Ex-PART] Compiling full design to Verilog..."
    compileFullToVerilog

synthesizeMonolithic :: IO ()
synthesizeMonolithic = do
    putStrLn "[Ex-PART] Synthesizing full design..."
    Yosys.synthesizeMonolithic

noConstraintPnR :: FilePath -> IO ()
noConstraintPnR lpfPath = do
    putStrLn "[Ex-PART] Performing place and route without expi constraints..."
    nextpnr lpfPath []


synthesizeAndPnRIndividualComponents :: FilePath -> IO ()
synthesizeAndPnRIndividualComponents lpfLoc = do
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
        -- Defined here as it is very specific logic to the flow
        -- _may_ be better in Yosys.hs and Nextpnr.hs, but since its only used here
        -- it's probably better ot keep it only here.
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

