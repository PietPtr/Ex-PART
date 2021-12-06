{-# LANGUAGE BangPatterns #-}

module Yosys where

import Types
import System.Directory
import System.Process
import System.Exit
import Data.Aeson
import GHC.IO.Handle

import Preprocessing
import Postprocessing
import Data.List.Split

synth_ecp5 :: String
synth_ecp5 = "synth_ecp5 -noccu2 -nomux -nobram -nodram -noflatten -nodsp"

compileToVerilog' :: [String] -> System -> IO ()
compileToVerilog' cmpNames top = do
    runClashParallel procsAndNames
    where
        procsAndNames = zip cmpNames (clashProcesses cmpNames)

compileToVerilog :: System -> IO ()
compileToVerilog top = do
    runClashParallel procsAndNames
    where
        cmpNames = map cmp_type (top_cmps $ sys_topdata top)
        procsAndNames = zip cmpNames (clashProcesses cmpNames)


-- TODO (lowprio): can be parallelized better by taking the transpose of batches and running all those sublist in a separate thread.
runClashParallel :: [(String, CreateProcess)] -> IO ()
runClashParallel procsAndNames = do
    mapM_ runClashes batches
    where
        batches = chunksOf 3 procsAndNames  -- Adjust this number if more cores can be used.
        
        runClashes procsAndNames = do
            handles <- mapM runClash procsAndNames
            mapM_ finishClashProcess handles
        
        finishClashProcess (cmpName, processHandle, outHandle, errHandle) = do
            stdout <- hGetContents outHandle
            stderr <- hGetContents errHandle
            code <- waitForProcess processHandle
            writeFile ("builds/"++cmpName++"/clash.log") stdout
            writeFile ("builds/"++cmpName++"/clash.err") stderr

            case code of
                ExitFailure code -> do
                    putStr $ "[Clash] " ++ stderr
                    error $ "Yosys.hs: Clash terminated with code " ++ show code
                ExitSuccess -> pure ()



-- assumes clash has been generated
compileFullToVerilog :: IO ()
compileFullToVerilog = do
    (_, Just outHandle, Just errHandle, processHandle) <- createProcess (proc "clash" [
            "Clash.hs",
            "--verilog",
            "-outputdir .hs",
            "-fclash-hdldir hdl"
        ]) {std_out=CreatePipe, std_err=CreatePipe}
    stdout <- hGetContents outHandle
    stderr <- hGetContents errHandle
    code <- waitForProcess processHandle
    writeFile ("clash.log") stdout
    writeFile ("clash.err") stderr

    case code of
        ExitFailure code -> do
            putStr $ "[Clash] " ++ stderr
            error $ "Yosys.hs: Clash terminated with code " ++ show code
        ExitSuccess -> pure ()

-- TODO (lowprio): Reorganise: Yosys.hs executes Clash, should be in clash/ somewhere.
-- TODO (lowprio): this pattern of process execution is repeated very often, and is contains the much output -> long runtime bug, build one generic version which streams such as nextpnr
runClash :: (String, CreateProcess) -> IO (String, ProcessHandle, Handle, Handle)
runClash (cmpName, clash) = do
    putStrLn $ "        | ...of component " ++ cmpName
    (_, Just outHandle, Just errHandle, processHandle) <- createProcess clash
    return (cmpName, processHandle, outHandle, errHandle)
    -- stdout <- hGetContents outHandle
    -- stderr <- hGetContents errHandle
    -- code <- waitForProcess processHandle
    -- writeFile ("builds/"++cmpName++"/clash.log") stdout
    -- writeFile ("builds/"++cmpName++"/clash.err") stderr

    -- case code of
    --     ExitFailure code -> do
    --         putStr $ "[Clash] " ++ stderr
    --         error $ "Yosys.hs: Clash terminated with code " ++ show code
    --     ExitSuccess -> pure ()


groupVerilogs :: System -> IO ()
groupVerilogs top = do
    createDirectoryIfMissing True $ "builds/.grouped"
    writeFile ("builds/.grouped/grouped.v") ""
    mapM_ (readAndAppend) (cmpNames)
    appendFile ("builds/.grouped/grouped.v") (dummyTop components)
    where
        cmpNames = map cmp_type components
        components = top_cmps $ sys_topdata top

synthesizeTop :: IO ()
synthesizeTop =
    runYosys ["/usr/share/ex-part/yosys/grouped.ys"]

synthesizeMonolithic :: IO ()
synthesizeMonolithic =
    runYosys ["/usr/share/ex-part/yosys/monolithic.ys"]

synthesizeHierarchic :: IO ()
synthesizeHierarchic =
    runYosys ["/usr/share/ex-part/yosys/hierarchic.ys"]


runYosys :: [String] -> IO ()
runYosys args = do
    (_, Just outHandle, Just errHandle, processHandle) <- createProcess $ 
        (proc "yosys" args)
        {std_err=CreatePipe, std_out=CreatePipe}
    stdout <- hGetContents outHandle
    stderr <- hGetContents errHandle
    writeFile "yosys.log" stdout
    writeFile "yosys.err" stderr
    code <- waitForProcess processHandle

    case code of
        ExitFailure code -> do
            putStr $ "[Yosys] " ++ stderr
            error $ "Yosys.hs: Yosys terminated with code " ++ show code
        ExitSuccess -> pure ()

customConnect :: System -> IO ()
customConnect top = 
    encodeFile "interconnect.json" (topModule)
    where
        topModule = makeTopModule top


combineJSONs :: String -> IO ()
combineJSONs basedir = do
    (_, _, _, h) <- createProcess $ 
        proc "python3" ["/usr/share/ex-part/yosys/merge_json.py", basedir]
    _ <- waitForProcess h
    return ()
