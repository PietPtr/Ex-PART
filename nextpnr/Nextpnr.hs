module Nextpnr where

import System.Process
import System.Exit

import GHC.IO.Handle

import Data.List

nextpnr :: FilePath -> IO ()
nextpnr lpf = do
    processDef <- pure $ (proc "nextpnr-ecp5"
        ["--85k",
         "--json", "combined.json",
         "--lpf", lpf,
         "--textcfg", "bitstream.config",
         "--write", "bitstream.json",
         "--pre-place", "/usr/share/ex-part/nextpnr/constrainer.py"
        --  "--pre-pack", "/usr/share/ex-part/nextpnr/constrainer.py", --"../debug/pre_pack.py",
        --  "--pre-route", "../debug/pre_route.py"
        ])
         {std_out=CreatePipe, std_err=CreatePipe}
    (_, Just outHandle, Just errHandle, processHandle) <- createProcess processDef
    stdout <- hGetContents outHandle
    stderr <- hGetContents errHandle
    code <- waitForProcess processHandle
    writeFile ("nextpnr.log") stdout
    writeFile ("nextpnr.err") stderr
    case code of
        ExitFailure code -> do
            putStr $ "[nextpnr] " ++ stdout
            error $ "nextpnr terminated with code " ++ show code
        ExitSuccess -> do
            putStr (unlines 
                $ filter (\l -> ("WARNING" `isPrefixOf` l) || ("ERROR" `isPrefixOf` l)) 
                $ lines stderr)
