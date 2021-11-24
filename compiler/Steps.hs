module Steps where

import Types

import Generator
import JSONBuilder
import Yosys
import Nextpnr

import Data.List
import Control.Concurrent
import System.Directory

createDirAndEnter :: FilePath -> IO ()
createDirAndEnter outDir = do
    putStrLn $ "[Ex-PART] Creating directory `" ++ outDir ++ "'..."
    createDirectoryIfMissing True outDir
    threadDelay 1000 -- This is terrible (:
    setCurrentDirectory outDir

writeLocations :: System -> IO ()
writeLocations system = do
    putStrLn "[Ex-PART] Generating locations.json..."
    writeLocationsJSON system

generateClash :: System -> IO ()
generateClash system = do
    putStrLn "[Ex-PART] Generating Clash code..."
    Generator.generateClash system

flattenForSim :: System -> IO ()
flattenForSim system = do
    putStrLn $ "[Ex-PART] Flattening design for Clash simulation..."
    flatten False system

compileToVerilog :: System -> IO ()
compileToVerilog system = do
    putStrLn "[Ex-PART] Compiling Clash code to Verilog..."
    Yosys.compileToVerilog system

groupVerilogFiles :: System -> IO ()
groupVerilogFiles top = do
    putStrLn "[Ex-PART] Grouping Verilog files into one file..."
    groupVerilogs top

synthesizeComponents :: IO ()
synthesizeComponents = do
    putStrLn "[Ex-PART] Synthesizing components to JSON..."
    synthesizeTop

connectComponents :: System -> IO ()
connectComponents top = do
    putStrLn "[Ex-PART] Connecting synthesized JSON according to expi file..."
    customConnect top

removeDeleted :: [Component] -> IO ()
removeDeleted deleted = do
    putStrLn "[Ex-PART] Removing deleted components..."
    mapM_ (\c -> removeDirectoryRecursive $ "builds/" ++ cmp_name c) deleted


flattenMonolithic :: System -> IO ()
flattenMonolithic system = do
    putStrLn $ "[Ex-PART] Flattening design for monolithic synthesis..."
    flatten False system

monolithicToVerilog :: IO ()
monolithicToVerilog = do
    putStrLn "[Ex-PART] Compiling full design to Verilog..."
    compileFullToVerilog

synthesizeMonolithic :: IO ()
synthesizeMonolithic = do
    putStrLn "[Ex-PART] Synthesizing monolithic design..."
    Yosys.synthesizeMonolithic

flattenHierarchic :: System -> IO ()
flattenHierarchic system = do
    putStrLn $ "[Ex-PART] Flattening design for hierarchic synthesis..."
    flatten True system

synthesizeHierarchic :: IO ()
synthesizeHierarchic = do
    putStrLn "[Ex-PART] Synthesizing hierarchic design..."
    Yosys.synthesizeHierarchic

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
        runToolOn :: String -> IO () -> IO ()
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

