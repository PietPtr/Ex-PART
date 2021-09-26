module Parser where

import Text.ParserCombinators.Parsec
import Parse_expi (system)
import Parse_expc (program)
import qualified Types


parse_expc :: FilePath -> IO (Either ParseError Types.Program)
parse_expc file = parse program "" <$> readFile file

parse_expi :: FilePath -> IO (Either ParseError Types.System)
parse_expi file = parse system "" <$> readFile file