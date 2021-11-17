module Constant where

import Types

-- We need to know the type of the constant, which can only be assumed to be the same as
-- the type of the component it is connected to...
generateComponent :: String -> Component
generateComponent constant = Component {
            cmp_name = "constant_" ++ identifierify constant,
            cmp_args = [],
            cmp_isoStats = [SOutput "out" ""]
        }

identifierify :: String -> String
identifierify s = concat $ map repl s
    where
        repl c
            | c `elem` (['0'..'9']++['a'..'z']++['A'..'Z']) = [c]
            | c `elem` ['(', ')'] = ""
            | otherwise = "_"