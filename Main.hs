module Main where

import Prelude hiding (repeat)

import Types
import Parser
import Repetition
import Generator
import JSONBuilder
import Yosys
import Nextpnr

import Text.ParserCombinators.Parsec
-- import Parse_expi

import Data.Either
import Data.Aeson
import Data.Text (pack, unpack)
import qualified Data.Map as Map
import Control.Monad
import Control.Concurrent
import System.Directory

-- expc :: String -> IO Program
expc prj = (parse_expc ("examples/" ++ prj ++ "/" ++ prj ++ ".expc"))

expi :: String -> IO System
expi prj = do
    prog <- expc prj
    case prog of
        (Right p) -> do
            syst <- parse_expi (prg_cmps p) $ "examples/" ++ prj ++ "/" ++ prj ++ ".expi"
            case syst of
                (Right s) -> pure s
                (Left err) -> do
                    putStrLn (show err)
                    return emptySystem
        (Left err) -> do
            putStrLn (show err)
            return emptySystem

-- TODO: uit elkaar trekken zodat verschillende fases los van elkaar gedaan kunnen worden?
flow :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
flow expcName expiName lpf outDir = do
    startDir <- getCurrentDirectory
    lpfLoc <- makeAbsolute lpf
    parsed <- parse_expc expcName
    case parsed of
        (Left error) -> putStrLn $ "[Ex-PART] Parse error: " ++ show error
        (Right result) -> putStrLn "[Ex-PART] Succesfully parsed .expc"
    guard (isRight parsed)
    expc <- pure $ fromRight undefined parsed

    parsed <- parse_expi (prg_cmps expc) expiName
    case parsed of
        (Left error) -> putStrLn $ "[Ex-PART] Parse error: " ++ show error
        (Right result) -> putStrLn "[Ex-PART] Succesfully parsed .expi"
    guard (isRight parsed)
    expi_reps <- pure $ fromRight undefined parsed

    putStrLn $ "[Ex-PART] Unrolling repeat & chain statements..."
    expi <- pure $ unroll expc expi_reps
    -- mapM_ print $ sys_repetitions expi
    -- mapM_ print $ map ins_name $ sys_instances expi
    -- mapM_ print $ sys_multicons expi
    -- mapM_ print $ sys_connections expi

    putStrLn $ "[Ex-PART] Creating directory `" ++ outDir ++ "`..."
    createDirectoryIfMissing True outDir
    threadDelay 1000 -- TODO: there is a dependence on these statements, but they're executed concurrently...
    setCurrentDirectory outDir

    putStrLn "[Ex-PART] Generating locations.json..."
    writeLocationsJSON expi

    putStrLn "[Ex-PART] Generating Clash code..."
    generateClash expc

    -- debug stuff
    -- setCurrentDirectory startDir
    -- error "done :)"
    -- debug stuff


    putStrLn "[Ex-PART] Compiling Clash code to Verilog..."
    compileToVerilog expc

    putStrLn "[Ex-PART] Grouping Verilog files into one file..."
    groupVerilogs expc

    putStrLn "[Ex-PART] Synthesizing components to JSON..."
    synthesizeTop

    putStrLn "[Ex-PART] Generating resource usage report..." 
    resourceReportJSON expc
    
    putStrLn $ "[Ex-PART] Flattening design for Clash simulation..."
    flatten expc expi

    putStrLn "[Ex-PART] Connecting synthesized JSON according to expi file..."
    customConnect expc expi

    putStrLn "[Ex-PART] Combining JSONs..."
    combineJSONs outDir

    putStrLn "[Ex-PART] Performing place and route using expi constraints..."
    nextpnr lpfLoc

    setCurrentDirectory startDir
    putStrLn $ "[Ex-PART] Done. Bitstream is " ++ outDir ++ "/bitstream.config "

make prj = flow
    (path ++ ".expc")
    (path ++ ".expi")
    (path ++ ".lpf")
    prj
    where
        path = "examples/" ++ prj ++ "/" ++ prj

collatz = make "collatz"
gol = make "gol"
repeat = make "repeat"
chain = make "chain"
up = setCurrentDirectory ".."

main = pure ()
