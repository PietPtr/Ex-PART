module Flattener where

import Types
import ComponentConversion

import qualified Data.Set as Set
import Data.Set (Set, union, unions)
import Data.List (intersperse)

-- Given any system, recursively generate one Clash component for the entire thing
flatten' :: System -> Program -> String
flatten' system program = flatten (system) (prg_cmps program)

flatten :: System -> [Component] -> String
flatten system allComponents = compDef
    where
        compDef = genComponentClash (usedComponentNames system) (allComponents)

-- flat :: System -> String
-- flat system = 
--     where
--         -- where_line instance 
        

usedComponentNames :: System -> Set String
usedComponentNames system = thisComps `union` otherComps
    where
        thisComps = Set.fromList $ map (cmp_name . ins_cmp) (sys_instances system) 
        otherComps = unions $ map usedComponentNames (sys_subsystems system)

genComponentClash :: Set String -> [Component] -> String
genComponentClash used comps = concat $ intersperse "\n" $ 
    (map toClash usedComps) ++ (map createMealy usedComps)
    where
        usedComps = filter (\c -> cmp_name c `elem` used) comps
        toClash cmp = concat $ intersperse "\n" $ 
            [ createTypeSignature cmp
            , createEquation cmp
            , createWhereClause cmp ]
