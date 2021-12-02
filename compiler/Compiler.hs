module Compiler where

import Types
import Parser
import Yosys
import Nextpnr
import qualified Flows
import Elaboration
import qualified Steps

import System.Directory
import Control.Monad
import Control.Concurrent
import System.FilePath


auto :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
auto expcPath expiPath lpfName outDir = do
    startDir <- getCurrentDirectory
    lpfLoc <- makeAbsolute lpfName

    design <- parse_both expcPath expiPath
    system <- pure $ elaborate design

    newExpiContent <- readFile expiPath

    outExists <- doesDirectoryExist outDir
    sourcesExist <- doesFileExist (outDir ++ "/build.expc")

    -- There are nicer ways to structure this control, but it works and will not be changed for now
    -- It _is_ easy to read too, as it is basically just very imperative...
    if (not outExists || not sourcesExist)
        then do
            putStrLn $ "[Ex-PART] No directory or old source files found, picking clean flow."
            Flows.clean system outDir
            finish lpfLoc outDir startDir
        else do
            putStrLn $ "[Ex-PART] Build directory exists, investigating expc changes..."
            setCurrentDirectory outDir

            let topdata = sys_topdata system
            oldDesign <- parse_expc "build.expc"
            defsAndTypes <- pure $ (top_defs topdata /= expcdes_defs oldDesign)
            if (defsAndTypes)
                then do
                    -- A bit overkill to go clean when the defs/combinatory change, but otherwise _very_ hard to find
                    -- what component to re-build.
                    putStrLn $ "[Ex-PART] Modified definitions or combinatory found, picking clean flow."
                    Flows.clean system outDir
                    finish lpfLoc outDir startDir
                else do
                    changed <- pure $ changedComponents (top_cmps topdata) (expcdes_cmps oldDesign)
                    deleted <- pure $ deletedComponents (top_cmps topdata) (expcdes_cmps oldDesign)
                    newcmps <- pure $ newComponents (top_cmps topdata) (expcdes_cmps oldDesign)
                    if (changed /= [] || deleted /= [] || newcmps /= []) 
                        then do
                            putStrLn $ "[Ex-PART] Changes in expc file found, picking expc flow."
                            Flows.expcChanged system changed deleted newcmps
                            finish lpfLoc outDir startDir
                        else do
                            putStrLn $ "[Ex-PART] No changes in expc file, investigating expi changes..."
                            oldExpiContent <- readFile "build.expi"
                            if (newExpiContent /= oldExpiContent)
                                then do
                                    putStrLn $ "[Ex-PART] Changes in expi file found, picking expi flow."
                                    Flows.expiChanged system
                                    finish lpfLoc outDir startDir
                                else do
                                    putStrLn "[Ex-PART] No changes to expi. Nothing to do. Finished."
                                    setCurrentDirectory startDir

    where
        finish lpfLoc outDir startDir = do
            putStrLn "[Ex-PART] Combining JSONs..."
            combineJSONs outDir

            Steps.constrainedNextpnr lpfLoc

            setCurrentDirectory startDir

            putStrLn $ "[Ex-PART] Copying source files to build directory..."
            copyFile expcPath (outDir ++ "/build.expc")
            copyFile expiPath (outDir ++ "/build.expi")

            putStrLn $ "[Ex-PART] Done. Bitstream is " ++ outDir ++ "/bitstream.config."


        

        changedComponents :: [Component] -> [Component] -> [Component]
        changedComponents [] _ = error "No components in expc file..."
        changedComponents _ [] = []
        changedComponents news (c:olds) = case other of
                Just c' -> c' : (changedComponents news olds)
                Nothing -> changedComponents news olds
            where
                other = case [c' | c'@Component {cmp_type=name} <- news, name == cmp_type c] of
                    [c'] -> if c' /= c then Just c' else Nothing
                    [] -> Nothing
                    _ -> error "Compiler.hs: How can there be several components with the same name?"
        
        deletedComponents :: [Component] -> [Component] -> [Component]
        deletedComponents news olds = [ c | c <- olds, not ((cmp_type c) `elem` (map cmp_type news)) ]
            
        newComponents :: [Component] -> [Component] -> [Component]
        newComponents news olds = [ c | c <- news, not ((cmp_type c) `elem` (map cmp_type olds)) ]


clean :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
clean expcPath expiPath lpfName outDir = do
    exists <- doesPathExist outDir
    when exists $ do
        mapM_ removeDirectoryRecursiveIfExists dirs
        files <- listDirectory outDir
        mapM_ removeFile (deleteFiles files)

    auto expcPath expiPath lpfName outDir
    where
        extensions = [".json", ".config", ".expc", ".expi", ".hs", ".err", ".log"]
        dirs = map (\f -> outDir ++ "/" ++ f) [".hs", "builds"]
        deleteFiles files = map (\f -> outDir ++ "/" ++ f) $
            filter (\f -> snd (splitExtension f) `elem` extensions) files

        removeDirectoryRecursiveIfExists :: FilePath -> IO ()
        removeDirectoryRecursiveIfExists path = do
            exists <- doesPathExist path
            when exists (removeDirectoryRecursive path)


monolithic :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
monolithic expcPath expiPath lpfPath outDir = do
    startDir <- getCurrentDirectory

    design <- parse_both expcPath expiPath
    system <- pure $ elaborate design

    Flows.monolithic system lpfPath outDir

    setCurrentDirectory startDir
    putStrLn $ "[Ex-PART] Done. Bitstream is " ++ outDir ++ "/bitstream.config."

hierarchic :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
hierarchic expcPath expiPath lpfPath outDir = do
    startDir <- getCurrentDirectory

    design <- parse_both expcPath expiPath
    system <- pure $ elaborate design

    Flows.hierarchic system lpfPath outDir

    setCurrentDirectory startDir
    putStrLn $ "[Ex-PART] Done. Bitstream is " ++ outDir ++ "/bitstream.config."


-- detailed usage stats can then be obtained by analyze.py or inspecting the logs.
resource :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
resource expcPath expiPath lpfPath outDir = do
    startDir <- getCurrentDirectory

    design <- parse_both expcPath expiPath
    system <- pure $ elaborate design

    Flows.resource system lpfPath outDir

    setCurrentDirectory startDir
    putStrLn $ "[Ex-PART] Finished processes for resource usage analysis"

resource' :: [String] -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
resource' components expcPath expiPath lpfPath outDir = do
    startDir <- getCurrentDirectory

    design <- parse_both expcPath expiPath
    system <- pure $ elaborate design

    Flows.resource' components system lpfPath outDir

    setCurrentDirectory startDir
    putStrLn $ "[Ex-PART] Finished processes for resource usage analysis"

-- TODO: abstract over an IO statement (partial, resource, monolithic, hierarhic)
location :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
location expcPath expiPath lpfPath outDir = do
    startDir <- getCurrentDirectory

    design <- parse_both expcPath expiPath
    system <- pure $ elaborate design

    Flows.location system outDir

    setCurrentDirectory startDir
    putStrLn $ "[Ex-PART] Finished processes for resource usage analysis"


sim :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
sim expcPath expiPath lpfPath outDir = do
    startDir <- getCurrentDirectory

    design <- parse_both expcPath expiPath
    system <- pure $ elaborate design

    Flows.sim system outDir

    setCurrentDirectory startDir
    putStrLn $ "[Ex-PART] Finished flattening the design for simulation"

-- TODO: nextpnr only flow
pnr :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
pnr expcPath expiPath lpfPath outDir = do
    startDir <- getCurrentDirectory
    lpfLoc <- makeAbsolute lpfPath

    Flows.pnr outDir lpfLoc

    setCurrentDirectory startDir
    putStrLn $ "[Ex-PART] Finished running nextpnr"

type Flow = (FilePath -> FilePath -> FilePath -> String -> IO ())

-- Easy helper function for consistently named projects
-- TODO (lowprio): There exists some function to execute an IO action within a directory, and even if it fails it returns to the original directory. Use that instead of setCurrentDirectory.
make :: Flow -> String -> IO ()
make flow prj = do
    let path = "examples/" ++ prj ++ "/" ++ prj
    expcPath <- makeAbsolute (path ++ ".expc")
    expiPath <- makeAbsolute (path ++ ".expi")
    lpfPath <- makeAbsolute (path ++ ".lpf")
    flow expcPath expiPath lpfPath prj

make_cmh :: String -> IO ()
make_cmh name = do
    putStrLn $ "[Ex-PART] running clean, monolithic, and hierarchic flow for " ++ name ++ "."
    mapM ((flip make) name) [clean, monolithic, hierarchic]
    putStrLn $ "[Ex-PART] Finished running clean, monolithic, and hierarchic flow for " ++ name ++ "."