{-# LANGUAGE OverloadedStrings #-}

module Locations where

import Data.Aeson
import Debug.Trace
import Data.Text (pack)
import Data.Graph hiding (Node)
import Data.Array

import Types

type X = Integer
type Y = Integer
data Pos = Pos X Y
    deriving Show

instance Num Pos where
    (+) (Pos x y) (Pos x' y') = Pos (x+x') (y+y')
    (-) (Pos x y) (Pos x' y') = Pos (x-x') (y-y')
    (*) (Pos x y) (Pos x' y') = Pos (x*x') (y*y')
    abs (Pos x y) = Pos (abs x) (abs y)
    signum (Pos x y) = 1 -- Nobody needs this right..?
    fromInteger x = (Pos x 0)

data AbsolutePositionTree 
    = Leaf String Pos Pos
    | Node String [AbsolutePositionTree]
    deriving Show

instance ToJSON Pos where
    toJSON (Pos x y) = object ["x" .= x, "y" .= y]

instance ToJSON AbsolutePositionTree where
    toJSON (Leaf name tl br) = object [ pack name .= object ["tl" .= tl, "br" .= br]]
    toJSON (Node name children) = object [(pack name) .= toJSON children]
    

relToAbs :: [(String, Size, Pos)] -> Pos -> System -> AbsolutePositionTree
relToAbs positions current system = Node (sys_id system) $ leaves ++ subsystems
    where
        leaves = map makeLeaf $ map ins_name $ sys_instances system
        subsystems = map nextCall $ sys_subsystems system
        nextCall subsystem = relToAbs positions (current + subsyspos) subsystem
            where
                subsyspos = (\(_, _, a) -> a) $ head $ filter (\(name, _, _) -> name == sys_id subsystem) positions

        makeLeaf :: String -> AbsolutePositionTree
        makeLeaf name = Leaf name tl br
            where
                tl = (pos + current)
                br = tl + (Pos (fst size - 1) (snd size - 1))
                (_, size, pos) = head $ filter (\(name', _, _) -> name' == name) positions

reduceAll' :: [(String, Size, Coords)] -> [(String, Size, Coords)] -> [(String, Size, Pos)]
reduceAll' [] _  = []
reduceAll' (inst:instances) all = solve inst : (reduceAll' instances all)
    where
        solve :: (String, Size, Coords) -> (String, Size, Pos)
        solve (name, size, (cx, cy)) = (name, size, Pos (reduceCoordExpr all cx) (reduceCoordExpr all cy))

reduceAll :: [(String, Size, Coords)] -> [(String, Size, Pos)]
reduceAll all = reduceAll' all all

reduceCoordExpr :: [(String, Size, Coords)] -> CoordExpr -> Integer
reduceCoordExpr instances cexpr = reduce $ reduceCoordsToConsts constantInstances cexpr
    where
        constantInstances = map (\(name, size, (cx, cy)) -> 
            (name, size, (reduceCoordsToConsts instances cx, reduceCoordsToConsts instances cy)))
            instances
        
        reduce :: CoordExpr -> Integer
        reduce (CAdd ce ce') = reduce ce + reduce ce'
        reduce (CConst c) = c
        reduce (CWidth id) = lookupWidth id
        reduce (CHeight id) = lookupHeight id
        reduce expr = error $ "Coordinate reduction found non-constant value ("++ (show expr) ++ "), good luck debugging! (JSONBuilder.hs, reduceCoords)"

        lookupWidth :: String -> Integer
        lookupWidth id = (\(_, (w, _), _) -> w) $ findID constantInstances id
        lookupHeight :: String -> Integer
        lookupHeight id = (\(_, (_, h), _) -> h) $ findID constantInstances id

reduceCoordsToConsts :: [(String, Size, Coords)] -> CoordExpr -> CoordExpr
reduceCoordsToConsts instances cexpr = case cexpr of
    (CAdd ce ce') -> (CAdd (reduceCoordsToConsts instances ce) (reduceCoordsToConsts instances ce'))
    (CX id) -> (\(_, _, (xexpr, _)) -> reduceCoordsToConsts instances xexpr) $ findID instances id
    (CY id) -> (\(_, _, (_, yexpr)) -> reduceCoordsToConsts instances yexpr) $ findID instances id 
    others -> others

        

cyclegraph = [("a", ["b"]), ("b", ["c"]), ("c", ["c"])]

toGraphList :: [(String, Size, Coords)] -> [(Name, [Name])]
toGraphList [] = []
toGraphList ((name, _, (x, y)):rest) = (name, neighbors) : toGraphList rest
    where
        neighbors = references x ++ references y

        references expr = case expr of
            (CAdd l r) -> references l ++ references r
            (CWidth name) -> [name]
            (CHeight name) -> [name]
            (CX name) -> [name]
            (CY name) -> [name]
            _ -> []

toGraph :: [(Name, [Name])] -> Graph
toGraph graphlist = graph
    where
        (graph, _, vertexFromKey) = graphFromEdges (map (\(n, l) -> (n, n, l)) graphlist)

isInCycle :: Graph -> Vertex -> [Bool]
isInCycle graph vertex = map (\v -> vertex `elem` (reachable graph v)) reachables
    where
        (_:reachables) = reachable graph vertex
        -- BUG? I assume here that the first node is always the node currently being looked at (trivially reachable)

-- V2: return where exactly the cycle was found, and what the cycle is.
hasCycle :: Graph -> Bool
hasCycle graph = (or $ concat $ map (isInCycle graph) nodes) || hasLoop
    where
        nodes = vertices graph

        hasLoop = any (\(v, ns) -> v `elem` ns) (assocs graph)


-- IDEA: iets van hashmaps doen hier? (premature optimization)
findID :: [(String, Size, Coords)] -> String -> (String, Size, Coords)
findID instances id = case filter (\(name, _, _) -> name == id) instances of
    (x:_) -> x
    [] -> error $ "Could not find ID " ++ id ++ " in provided list." ++ show instances


allInstsWithCoords :: System -> [(String, Size, Coords)]
allInstsWithCoords system = 
    [(sys_id system, sys_size system, sys_coords system)] ++
    -- (map (addCoords $ sys_coords system) $
    map tuplize (sys_instances system) ++ 
    subInstances
    where
        tuplize ins = (ins_name ins, ins_size ins, ins_coords ins)
        subInstances = concat $ map allInstsWithCoords (sys_subsystems system)

        addCoords :: Coords -> (String, Size, Coords) -> (String, Size, Coords)
        addCoords coords' (name, size, coords) = (name, size, 
            (CAdd (fst coords) (fst coords'),
             CAdd (snd coords) (snd coords')))

-- ToJSON the APT