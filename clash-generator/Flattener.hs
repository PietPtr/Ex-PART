module Flattener where

import Types
import ComponentConversion
import Preliminary

import qualified Data.Set as Set
import Data.Set (Set, union, unions)
import Data.List (intersperse, intercalate)

-- Given any system, recursively generate one Clash project for the entire thing

flatten :: Program -> System -> String
flatten program system = intercalate "\n\n" $
    [ imports
    , compDef 
    , systems 
    ,topEntity ]
    where
        imports = "import Clash.Prelude\nimport Definitions\n\n"
        compDef = genComponentClash (usedComponentNames system) (prg_cmps program)
        systems = flatten' system
        topEntity = createSynthesizable (map io2iso $ sys_iodefs system) (sys_id system) False

flatten' :: System -> String
flatten' system = subsysDefs ++ "\n\n\n" ++ sysdef
    where
        subsysDefs = intercalate "\n\n" $
            map flatten' $ sys_subsystems system

        sysdef = typeDef system ++ definition system ++ "\n    where\n" ++ whereBlock system


typeDef :: System -> String
typeDef system = name ++ " :: HiddenClockResetEnable dom =>\n" ++
    "    Signal dom ("++ in_types ++") -> Signal dom ("++ out_types ++")\n"
    where
        name = sys_id system
        in_types = intercalate ", " $
            map (\(Input _ t) -> t) $ inputs' $ sys_iodefs system
        out_types = intercalate ", " $
            map (\(Output _ t) -> t) $ outputs' $ sys_iodefs system


definition :: System -> String
definition system = name ++ " input = "++ bundle ++"(" ++ out_str ++ ")"
    where
        name = sys_id system
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
             ++ (map (instanceWhereStatement (sys_connections system) (sys_constantDrivers system)) $ sys_instances system)
             ++ (map (systemWhereStatement (sys_connections system)) $ sys_subsystems system) 
             ++ (map (constantWhereStatement) (sys_constantDrivers system))


unpackedInput :: System -> String 
unpackedInput system = "        " ++ "(" ++ ins_str ++ ") = unbundle input"
    where
        iodefs = sys_iodefs system
        ins_str = if length (inputs' iodefs) == 0
            then "no_input"
            else concat $ intersperse ", " $ 
                map (varName' "this") $ inputs' $ iodefs


instanceWhereStatement :: [Connection] -> [ConstantDriver] -> Instance -> String
instanceWhereStatement conns consts inst = whereStatement ins outs (cmpName ++ "M")
    where
        component = ins_cmp inst
        cmpName = cmp_name component
        name = ins_name inst
        ins = map findConn $ inputs $ cmp_isoStats component
        outs = map (\(SOutput portName _) -> name ++ "_" ++ portName)
            $ outputs $ cmp_isoStats component

        findConn :: ISOStat -> String
        findConn (SInput portname _) = case filter f conns of
            (c:_) -> varName c
            _ -> case filter g consts of
                ((ConstantDriver value _):_) -> "const_" ++ value
                _ -> error $ "Flattener.hs: No connection specified for component " ++ 
                    name ++ " (is " ++ cmpName ++ "), port `" ++ portname ++ "`"
            where
                f (Connection (CID _ _) (CID inst_name' portname')) = 
                    inst_name' == ins_name inst && 
                    portname == portname'
                
                g (ConstantDriver value (CID inst_name' portname')) = 
                    inst_name' == ins_name inst &&
                    portname == portname'



-- TODO (elab): could be neater with an abstraction over IO/ISO statement and handling connection finding as such
systemWhereStatement :: [Connection] -> System -> String
systemWhereStatement conns system = whereStatement ins outs name
    where
        name = sys_id system
        ins = map varName $ map (findIOConn conns name) $ inputs' $ sys_iodefs system
        outs = map (varName' $ sys_id system) $ outputs' $ sys_iodefs system

constantWhereStatement :: ConstantDriver -> String
constantWhereStatement (ConstantDriver value cid) = 
    "        const_" ++ value ++ " = pure " ++ value


findIOConn :: [Connection] -> String -> IOStat -> Connection
findIOConn conns sysid io = 
    case filter f conns of
        (c:_) -> c
        _ -> error $ "Flattener.hs: No connection specified for io statement " ++ show io ++ " in system " ++ sysid
    where
        f (Connection (CID _ _) (CID sys_name' portname')) = 
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


varName :: Connection -> String
varName (Connection (CID inst portname) _) = inst ++ "_" ++ portname

varName' :: String -> IOStat -> String
varName' sys io = sys ++ "_" ++ portname io




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

