module Parser where

import Text.ParserCombinators.Parsec
import Parse_expi (system)
import Parse_expc (expcdesign)
import Types
import Data.Either
import Control.Monad


parse_expc :: FilePath -> IO ExpcDesign
parse_expc file = do
    parsed <- parse expcdesign "" <$> readFile file
    case parsed of
        (Left errMsg) -> putStrLn $ "[Ex-PART] Parse error: " ++ show errMsg
        (Right _) -> putStrLn $ "[Ex-PART] Succesfully parsed " ++ file
    guard (isRight parsed)
    return $ fromRight undefined parsed


parse_expi :: ExpcDesign -> FilePath -> IO Design
parse_expi expc file = do
    parsed <- parse (system $ expcdes_cmps expc) "" <$> readFile file
    case parsed of
        (Left errMsg) -> putStrLn $ "[Ex-PART] Parse error: " ++ show errMsg
        (Right _) -> putStrLn $ "[Ex-PART] Succesfully parsed " ++ file
    guard (isRight parsed)
    return Design {
            des_defs = expcdes_defs expc,
            des_cmbs = expcdes_cmbs expc,
            des_cmps = expcdes_cmps expc,
            des_systree = fromRight undefined parsed
        }

parse_both :: FilePath -> FilePath -> IO Design
parse_both expc_file expi_file = do
    expcDesign <- parse_expc expc_file
    parse_expi expcDesign expi_file