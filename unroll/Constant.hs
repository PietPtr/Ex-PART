module Constant where

import Types
import Data.List



-- makeConstantConnections :: [Connection] -> [Connection]
-- makeConstantConnections [] = []
-- makeConstantConnections (conn:conns) = case conn of
--     (Connection (ConstantDriver str) cid) -> 
--         (Connection (CID (identifier str) "const") cid) : (makeConstantConnections conns)
--     conn -> (makeConstantConnections conns)
--     where
--         identifier s = "$const_" ++ show (read s :: Int)

 
identifierify :: String -> String
identifierify s = concat $ map repl s
    where
        repl c
            | c `elem` (['0'..'9']++['a'..'z']++['A'..'Z']) = [c]
            | c `elem` ['(', ')'] = ""
            | otherwise = "_"

