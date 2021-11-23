module Compiler where

import Types
import Parser
import Unroll
import Generator
import JSONBuilder
import Yosys
import Nextpnr
import qualified Flows

import Data.Either
import Data.Aeson
import Data.Text (pack, unpack)
import qualified Data.Map as Map
import Control.Monad
import Control.Concurrent
import System.Directory
import Data.List.Split



auto :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
auto expcPath expiPath lpfName outDir = do
    startDir <- getCurrentDirectory
    lpfLoc <- makeAbsolute lpfName

    expc <- parse parse_expc expcPath
    expi_reps <- parse (parse_expi $ prg_cmps expc) expiPath
    newExpiContent <- readFile expiPath

    outExists <- doesDirectoryExist outDir
    sourcesExist <- doesFileExist (outDir ++ "/build.expc")

    -- TODO: there are nicer ways to structure this control right?
    if (not outExists || not sourcesExist)
        then do
            putStrLn $ "[Ex-PART] No directory or old source files found, picking clean flow."
            Flows.clean expc expi_reps expcPath expiPath outDir
            finish lpfLoc outDir startDir
        else do
            putStrLn $ "[Ex-PART] Build directory exists, investigating expc changes..."
            setCurrentDirectory outDir

            oldExpc <- parse parse_expc "build.expc"
            changed <- pure $ changedComponents (prg_cmps expc) (prg_cmps oldExpc)
            deleted <- pure $ deletedComponents (prg_cmps expc) (prg_cmps oldExpc)
            newcmps <- pure $ newComponents (prg_cmps expc) (prg_cmps oldExpc)
            if (changed /= [] || deleted /= [] || newcmps /= []) 
                then do
                    putStrLn $ "[Ex-PART] Changes in expc file found, picking expc flow."
                    Flows.expcChanged expc expi_reps changed deleted newcmps
                    finish lpfLoc outDir startDir
                else do
                    putStrLn $ "[Ex-PART] No changes in expc file, investigating expi changes... "
                    oldExpiContent <- readFile "build.expi"
                    if (newExpiContent /= oldExpiContent)
                        then do
                            putStrLn $ "[Ex-PART] Changes in expi file found, picking expi flow."
                            Flows.expiChanged expc expi_reps
                            finish lpfLoc outDir startDir
                        else do
                            putStrLn "[Ex-PART] No changes to expi. Nothing to do. Finished."
                            setCurrentDirectory startDir

    where
        finish lpfLoc outDir startDir = do
            putStrLn "[Ex-PART] Combining JSONs..."
            combineJSONs outDir

            putStrLn "[Ex-PART] Performing place and route using expi constraints..."
            nextpnr lpfLoc ["--pre-place", "/usr/share/ex-part/nextpnr/constrainer.py"]

            setCurrentDirectory startDir

            putStrLn $ "[Ex-PART] Copying source files to build directory..."
            copyFile expcPath (outDir ++ "/build.expc")
            copyFile expiPath (outDir ++ "/build.expi")

            putStrLn $ "[Ex-PART] Done. Bitstream is " ++ outDir ++ "/bitstream.config."


        

        changedComponents :: [Component] -> [Component] -> [Component]
        changedComponents [] olds = error "No components in expc file..."
        changedComponents news [] = []
        changedComponents news (c:olds) = case other of
                Just c' -> c' : (changedComponents news olds)
                Nothing -> changedComponents news olds
            where
                other = case [c' | c'@Component {cmp_name=name} <- news, name == cmp_name c] of
                    [c'] -> if c' /= c then Just c' else Nothing
                    [] -> Nothing
                    _ -> error "Compiler.hs: How can there be several components with the same name?"
        
        deletedComponents :: [Component] -> [Component] -> [Component]
        deletedComponents news olds = [ c | c <- olds, not ((cmp_name c) `elem` (map cmp_name news)) ]
            
        newComponents :: [Component] -> [Component] -> [Component]
        newComponents news olds = [ c | c <- news, not ((cmp_name c) `elem` (map cmp_name olds)) ]

parse :: Show a => (FilePath -> IO (Either a b)) -> FilePath -> IO b
parse parser path = do
    parsed <- parser path
    case parsed of
        (Left error) -> putStrLn $ "[Ex-PART] Parse error: " ++ show error
        (Right result) -> putStrLn $ "[Ex-PART] Succesfully parsed " ++ path
    guard (isRight parsed)
    pure $ fromRight undefined parsed

clean :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
clean expcPath expiPath lpfName outDir = do
    exists <- doesPathExist outDir
    if exists
        then removeDirectoryRecursive outDir
        else pure ()
    auto expcPath expiPath lpfName outDir

monolithic :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
monolithic expcPath expiPath lpfPath outDir = do
    -- TODO: doesn't absolutize paths, whose responsibility is that?
    startDir <- getCurrentDirectory
    expc <- parse parse_expc expcPath
    expi_reps <- parse (parse_expi $ prg_cmps expc) expiPath

    Flows.monolithic expc expi_reps lpfPath outDir

    setCurrentDirectory startDir
    putStrLn $ "[Ex-PART] Done. Bitstream is " ++ outDir ++ "/bitstream.config."

-- TODO: usage stats kunnen ook weer uit logs geplukt worden 
resource :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
resource expcPath expiPath lpfPath outDir = do
    startDir <- getCurrentDirectory
    expc <- parse parse_expc expcPath

    Flows.resource expc lpfPath outDir

    setCurrentDirectory startDir
    putStrLn $ "[Ex-PART] Finished processes for resource usage analysis"


-- Easy helper function for consistently named projects
make flow prj = do
    let path = "examples/" ++ prj ++ "/" ++ prj
    expcPath <- makeAbsolute (path ++ ".expc")
    expiPath <- makeAbsolute (path ++ ".expi")
    lpfPath <- makeAbsolute (path ++ ".lpf")
    flow expcPath expiPath lpfPath prj