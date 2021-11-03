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

    outExists <- doesDirectoryExist outDir

    if not outExists
        then do
            Flows.clean expc expi_reps expcPath expiPath outDir
            finish lpfLoc outDir startDir
        else do
            putStrLn $ "[Ex-PART] Build directory exists, investigating changes..."
            setCurrentDirectory outDir
            oldExpc <- parse parse_expc "build.expc"
            changed <- pure $ changedComponents (prg_cmps expc) (prg_cmps oldExpc)
            mapM_ print (map cmp_name changed)



    where
        finish lpfLoc outDir startDir = do
            putStrLn "[Ex-PART] Performing place and route using expi constraints..."
            nextpnr lpfLoc

            setCurrentDirectory startDir
            putStrLn $ "[Ex-PART] Done. Bitstream is " ++ outDir ++ "/bitstream.config "

        parse parser path = do
            parsed <- parser path
            case parsed of
                (Left error) -> putStrLn $ "[Ex-PART] Parse error: " ++ show error
                (Right result) -> putStrLn "[Ex-PART] Succesfully parsed .expi"
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
        -- vergeet niet een keer een lijstje te maken van deleted components, daar moet ook wat mee
             




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