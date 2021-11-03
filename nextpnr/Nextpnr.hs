module Nextpnr where

import System.Process
import System.Exit

import GHC.IO.Handle

import Data.List

nextpnr :: Bool -> FilePath -> IO ()
nextpnr constrain lpf = do
    print "LPF is dit:"
    print lpf
    processDef <- pure $ (proc "/home/pieter/Education/Thesis/stables/nextpnr/nextpnr-ecp5" $
        ["--85k",
         "--json", "synthesized.json",
         "--lpf", lpf,
         "--textcfg", "bitstream.config",
         "--write", "bitstream.json",
         "--lpf-allow-unconstrained" -- TODO: this should be off by default
        ] ++ constrainOptions)
         {std_out=CreatePipe, std_err=CreatePipe}
    (_, Just outHandle, Just errHandle, processHandle) <- createProcess processDef
    stdout <- hGetContents outHandle
    stderr <- hGetContents errHandle
    code <- waitForProcess processHandle
    writeFile ("nextpnr.log") stdout
    writeFile ("nextpnr.err") stderr
    case code of
        ExitFailure code -> do
            putStr $ "[nextpnr] " ++ warnsAndErrors stderr
            error $ "nextpnr terminated with code " ++ show code
        ExitSuccess -> do
            putStr (warnsAndErrors stderr)
    where
        warnsAndErrors stderr = (unlines 
                $ filter (\l -> ("WARNING" `isPrefixOf` l) || ("ERROR" `isPrefixOf` l)) 
                $ lines stderr)

        constrainOptions = if constrain
            then ["--pre-place", "/usr/share/ex-part/nextpnr/constrainer.py"]
            else []
