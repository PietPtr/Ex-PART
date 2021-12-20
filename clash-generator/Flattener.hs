module Flattener where

import Types
import ComponentConversion

import qualified Data.Set as Set
import Data.Set (Set, union, unions)
import Data.List (intersperse, intercalate, nubBy)
import Data.Maybe
import Debug.Trace

-- Given any system, recursively generate one Clash project for the entire thing

flattenMonolithic :: System -> String
flattenMonolithic = flatten False

flattenHierarchic :: System -> String
flattenHierarchic = flatten True

flatten :: Bool -> System -> String
flatten inline top = intercalate "\n\n" $
    [ imports
    , inlinedefs
    , compDef
    , systems 
    ,topEntity ]
    where
        imports = "import Clash.Prelude\nimport Definitions\nimport Debug.Trace\nimport qualified Data.List as L\nimport Data.Maybe\n\n"
        compDef = genComponentClash names (top_cmps $ sys_topdata top)
        systems = flatten' inline top
        topEntity = createSynthesizable (map io2iso $ sys_iodefs top) (sys_name top) False

        names = (usedComponentNames top)

        inlinedefs = if inline
            then intercalate "\n" (map (\n -> noinline (n ++ "M")) (Set.toList names))
            else "-- Monolithic file, everything is inlined by default."

flatten' :: Bool -> System -> String
flatten' inline system = sysdef
    where
        subsysDefs = intercalate "\n\n" $
            map (flatten' inline) (sys_subsystems system)

        sysdef = 
            -- inlineDef ++
            -- typeDef system ++
            --  definition system ++ 
            --  "\n    where\n" ++ 
            --  whereBlock system ++ "\n" ++
            --  indent subsysDefs 
             -- TODO: this is not the best solution to prevent re-used systems clashing in scope of course...
            inlineDef ++
            typeDef system ++
            definition system ++ 
            "\n    where\n" ++ 
            whereBlock system ++ "\n" ++
            subsysDefs 

        indent str = unlines $ map ("        " ++ ) $ lines str
        
        inlineDef = if inline
            then noinline (sys_name system)
            else []

noinline :: String -> String
noinline name = "{-# NOINLINE " ++ name ++ " #-}\n"

typeDef :: System -> String
typeDef system = name ++ " :: HiddenClockResetEnable dom =>\n" ++
    "    Signal dom ("++ in_types ++") -> Signal dom ("++ out_types ++")\n"
    where
        name = sys_name system
        in_types = intercalate ", " $
            map (\(Input _ t) -> t) $ inputs' $ sys_iodefs system
        out_types = intercalate ", " $
            map (\(Output _ t) -> t) $ outputs' $ sys_iodefs system


definition :: System -> String
definition system = name ++ " input = "++ bundle ++"(" ++ out_str ++ ")"
    where
        name = sys_name system
        out_str = intercalate ", " $ map varName $ 
            map (findIOConn (sys_connections system) "this") $ outputs
        bundle = if length outputs > 1
            then "bundle $ "
            else ""

        outputs = outputs' $ sys_iodefs system


whereBlock :: System -> String
whereBlock system = concat $ intersperse "\n" stats
    where
        stats = [unpackedInput system]
            ++ (map (elementWhereStatement (sys_connections system) (sys_constantDrivers system)) $ sys_elems system)
            ++ (map (constantWhereStatement) uniqueDrivers)

        uniqueDrivers = nubBy equalDrivers (sys_constantDrivers system)
        equalDrivers (ConstantDriver v1 _) (ConstantDriver v2 _) = v1 == v2

unpackedInput :: System -> String 
unpackedInput system = "        " ++ "(" ++ ins_str ++ ") = "++ unbundle ++" input"
    where
        iodefs = sys_iodefs system
        ins_str = if length (inputs' iodefs) == 0
            then "no_input"
            else concat $ intersperse ", " $ 
                map (varName' "this") $ inputs' $ iodefs
            
        unbundle = if length (inputs' iodefs) > 1
            then "unbundle"
            else ""

elementWhereStatement :: [Connection'] -> [ConstantDriver] -> Element -> String
elementWhereStatement conns consts elem = whereStatement ins outs clashName
    where
        clashName = if elem_isSystem elem
            then elem_name elem
            else elem_type elem ++ "M"
        
        ins = map findConn $ inputs' $ elem_iodefs elem
        outs = map (varName' $ elem_name elem) $ outputs' $ elem_iodefs elem
        
        findConn :: IOStat -> String
        findConn (Input portname _) = case filter equal conns of
            (c:_) -> varName c
            _ -> case filter driving consts of
                ((ConstantDriver value _):_) -> tail constPrefix ++ value
                _ -> error $ "Flattener.hs: No connection specified for element " ++ 
                    elem_name elem ++ " (is " ++ elem_type elem ++ "), port `" ++ portname ++ "`"
            where
                equal (Connection' (CID _ _) (CID elem_name' portname') _) = 
                    elem_name' == elem_name elem && 
                    portname == portname'

                driving (ConstantDriver _ (CID elem_name' portname')) = 
                    elem_name' == elem_name elem &&
                    portname == portname'


constantWhereStatement :: ConstantDriver -> String
constantWhereStatement (ConstantDriver value _) = 
    "        const_" ++ value ++ " = pure " ++ value

findIOConn :: [Connection'] -> String -> IOStat -> Connection'
findIOConn conns sysid io = 
    case filter equal conns of
        (c:_) -> c
        _ -> error $ "Flattener.hs: No connection specified for io statement " ++ show io ++ " in system " ++ sysid ++ "\n" ++ (unlines $ map show conns)
    where
        equal (Connection' (CID _ _) (CID sys_name' portname') _) = 
            sys_name' == sysid && 
            portname' == (portname io)

portname :: IOStat -> String
portname io = case io of
    (Input p _) -> p
    (Output p _) -> p

whereStatement :: [String] -> [String] -> String -> String
whereStatement ins outs name = "        " ++ 
    outs_str ++ " = " ++ 
    unbundle ++ name ++ " " ++
    bundle   ++ in_str
    where
        in_str = "(" ++ input_tuple_content ++ ")"
        input_tuple_content = if length ins == 0
            then "pure ()"
            else (concat $ intersperse ", " ins)
        bundle = if length ins > 1
            then "$ bundle "
            else ""

        outs_str = "(" ++ (concat $ intersperse ", " outs) ++ ")"
        unbundle = if length outs > 1
            then "unbundle $ "
            else ""


varName :: Connection' -> String
varName (Connection' (CID inst portname) _ _) = inst ++ "_" ++ portname

varName' :: String -> IOStat -> String
varName' sys io = sys ++ "_" ++ portname io

usedComponentNames :: System -> Set String
usedComponentNames system = 
    (Set.fromList $ mapMaybe component_name (sys_elems system)) 
    `union`
    (unions $ map usedComponentNames (sys_subsystems system))
    where
        component_name elem = case elem_implementation elem of
            (InstanceImpl inst) -> Just $ cmp_type $ cins_cmp inst
            _ -> Nothing

genComponentClash :: Set String -> [Component] -> String
genComponentClash used comps = intercalate "\n" $ 
    (map toClash usedComps) ++ (map createMealy usedComps)
    where
        usedComps = filter (\c -> cmp_type c `elem` used) comps
        toClash cmp = intercalate "\n" $ 
            [ createTypeSignature cmp
            , createEquation cmp
            , createWhereClause cmp ]

