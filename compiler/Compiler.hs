module Compiler where

import Types
import Parser
import Yosys
import Nextpnr
import qualified Flows
import Elaboration

import System.Directory



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
            defsAndTypes <- pure $ (top_defs topdata /= expcdes_defs oldDesign) || (top_cmbs topdata /= expcdes_cmbs oldDesign)
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

            putStrLn "[Ex-PART] Performing place and route using expi constraints..."
            nextpnr lpfLoc ["--pre-place", "/usr/share/ex-part/nextpnr/constrainer.py"]

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
                other = case [c' | c'@Component {cmp_name=name} <- news, name == cmp_name c] of
                    [c'] -> if c' /= c then Just c' else Nothing
                    [] -> Nothing
                    _ -> error "Compiler.hs: How can there be several components with the same name?"
        
        deletedComponents :: [Component] -> [Component] -> [Component]
        deletedComponents news olds = [ c | c <- olds, not ((cmp_name c) `elem` (map cmp_name news)) ]
            
        newComponents :: [Component] -> [Component] -> [Component]
        newComponents news olds = [ c | c <- news, not ((cmp_name c) `elem` (map cmp_name olds)) ]


clean :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
clean expcPath expiPath lpfName outDir = do
    exists <- doesPathExist outDir
    if exists
        then removeDirectoryRecursive outDir
        else pure ()
    auto expcPath expiPath lpfName outDir


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

type Flow = (FilePath -> FilePath -> FilePath -> String -> IO ())

-- Easy helper function for consistently named projects
make :: Flow -> String -> IO ()
make flow prj = do
    let path = "examples/" ++ prj ++ "/" ++ prj
    expcPath <- makeAbsolute (path ++ ".expc")
    expiPath <- makeAbsolute (path ++ ".expi")
    lpfPath <- makeAbsolute (path ++ ".lpf")
    flow expcPath expiPath lpfPath prj