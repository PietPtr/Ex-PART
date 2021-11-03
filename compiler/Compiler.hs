module Compiler where

import Types
import Parser
import Repetition
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
            putStrLn $ "[Compile] No directory or old source files found, picking clean flow."
            Flows.clean expc expi_reps expcPath expiPath outDir
            finish lpfLoc outDir startDir
        else do
            putStrLn $ "[Compile] Build directory exists, investigating expc changes..."
            setCurrentDirectory outDir

            oldExpc <- parse parse_expc "build.expc"
            changed <- pure $ changedComponents (prg_cmps expc) (prg_cmps oldExpc)
            deleted <- pure $ deletedComponents (prg_cmps expc) (prg_cmps oldExpc)
            newcmps <- pure $ newComponents (prg_cmps expc) (prg_cmps oldExpc)
            if (changed /= [] || deleted /= [] || newcmps /= []) 
                then do
                    putStrLn $ "[Compile] Changes in expc file found, picking expc flow."
                    Flows.expcChanged expc expi_reps changed deleted newcmps
                    finish lpfLoc outDir startDir
                else do
                    putStrLn $ "[Compile] No changes in expc file, investigating expi changes... "
                    oldExpiContent <- readFile "build.expi"
                    if (newExpiContent /= oldExpiContent)
                        then do
                            putStrLn $ "[Compile] Changes in expi file found, picking expi flow."
                            Flows.expiChanged expc expi_reps
                            finish lpfLoc outDir startDir
                        else do
                            putStrLn "[Compile] No changes to expi. Nothing to do. Finished."

    where
        finish lpfLoc outDir startDir = do
            putStrLn "[Ex-PART] Combining JSONs..."
            combineJSONs outDir

            putStrLn "[Ex-PART] Performing place and route using expi constraints..."
            nextpnr lpfLoc

            setCurrentDirectory startDir

            putStrLn $ "[Ex-PART] Copying source files to build directory..."
            copyFile expcPath (outDir ++ "/build.expc")
            copyFile expiPath (outDir ++ "/build.expi")

            putStrLn $ "[Ex-PART] Done. Bitstream is " ++ outDir ++ "/bitstream.config."


        parse parser path = do
            parsed <- parser path
            case parsed of
                (Left error) -> putStrLn $ "[Ex-PART] Parse error: " ++ show error
                (Right result) -> putStrLn $ "[Ex-PART] Succesfully parsed " ++ path
            guard (isRight parsed)
            pure $ fromRight undefined parsed

        -- TODO: maak dus van een cmp list een set?
        -- equalCmpLists :: [Component] -> [Component] -> Bool
        -- equalCmpLists [] _ = True -- TODO: weird or dangerous notion of equality here...
        -- equalCmpLists (c:cmps) cmps' = same && (equalCmpLists cmps cmps')
        --     where
        --         same = case [c' | c'@Component {cmp_name=name'} <- cmps', name' == cmp_name c] of
        --             [c'] -> c' == c
        --             [] -> False
        --             _ -> error "How can there be several components with the same name?"

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
                    _ -> error "How can there be several components with the same name?"
        
        deletedComponents :: [Component] -> [Component] -> [Component]
        deletedComponents news olds = [ c | c <- olds, not ((cmp_name c) `elem` (map cmp_name news)) ]
            
        newComponents :: [Component] -> [Component] -> [Component]
        newComponents news olds = [ c | c <- news, not ((cmp_name c) `elem` (map cmp_name olds)) ]




clean :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
clean = undefined


-- Easy helper function for consistently named projects
make prj = auto
    (path ++ ".expc")
    (path ++ ".expi")
    (path ++ ".lpf")
    prj
    where
        path = "examples/" ++ prj ++ "/" ++ prj