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
    readloop errHandle
    code <- waitForProcess processHandle
    writeFile ("nextpnr.log") stdout
    case code of
        ExitFailure code -> do
            error $ "Nextpnr.hs: nextpnr terminated with code " ++ show code
        ExitSuccess -> pure ()
    where
        warnsAndErrors stderr = (unlines 
                $ filter (\l -> ("WARNING" `isPrefixOf` l) || ("ERROR" `isPrefixOf` l)) 
                $ lines stderr)

        bitstreamOptions = if ("--out-of-context" `elem` options)
            then []
            else ["--textcfg", "bitstream.config"]

        readloop errHandle = do -- clear error log if exists, create if doesn't exist.
            eof <- hIsEOF errHandle
            if eof
                then putStrLn "[Ex-PART] nextpnr terminated."
                else do
                    line <- hGetLine errHandle
                    putStrLn $ "[nextpnr] " ++ line -- TODO (lowprio): allow filter spec?
                    appendFile "nextpnr.err" (line ++ "\n")
                    readloop errHandle