module Nextpnr where

import System.Process
import System.Exit

import GHC.IO.Handle

import Data.List

nextpnr :: FilePath -> [String] -> IO ()
nextpnr lpf options = do
    processDef <- pure $ (proc "/usr/share/ex-part/nextpnr/nextpnr-ecp5" $
        ["--85k",
         "--json", "synthesized.json",
         "--lpf", lpf,
         "--write", "bitstream.json",
         "--lpf-allow-unconstrained" -- TODO (lowprio): this should be off by default
        ] ++ options ++ bitstreamOptions)
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
            error $ "Nextpnr.hs: nextpnr terminated with code " ++ show code
        ExitSuccess -> do
            putStr (warnsAndErrors stderr)
    where
        warnsAndErrors stderr = (unlines 
                $ filter (\l -> ("WARNING" `isPrefixOf` l) || ("ERROR" `isPrefixOf` l)) 
                $ lines stderr)

        bitstreamOptions = if ("--out-of-context" `elem` options)
            then []
            else ["--textcfg", "bitstream.config"]
