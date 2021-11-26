{-# LANGUAGE OverloadedStrings #-}

module Locations where

import Data.Aeson
import Data.Text (pack)
import Data.Graph hiding (Node)
import Data.Array

import Types


-- Coords are reduced to Pos, which is absolute
type X = Integer
type Y = Integer

data Pos = Pos X Y
    deriving Show

-- Sizes are reduced to WH (width height), which are constant.
type W = Integer
type H = Integer
data WH = WH W H
    deriving Show

instance Num Pos where
    (+) (Pos x y) (Pos x' y') = Pos (x+x') (y+y')
    (-) (Pos x y) (Pos x' y') = Pos (x-x') (y-y')
    (*) (Pos x y) (Pos x' y') = Pos (x*x') (y*y')
    abs (Pos x y) = Pos (abs x) (abs y)
    signum (Pos _ _) = 1 -- Nobody needs this right..?
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
    

relToAbs :: [(String, WH, Pos)] -> Pos -> System -> AbsolutePositionTree
relToAbs positions current system = Node (sys_name system) $ leaves ++ subsystems
    where
        leaves = map makeLeaf $ map ins_name $ sys_instances system
        subsystems = map nextCall $ sys_subsystems system
        nextCall subsystem = relToAbs positions (current + subsyspos) subsystem
            where
                subsyspos = (\(_, _, a) -> a) $ head $ filter (\(name, _, _) -> name == sys_name subsystem) positions

        makeLeaf :: String -> AbsolutePositionTree
        makeLeaf name = Leaf name tl br
            where
                tl = (pos + current)
                br = tl + (Pos (w - 1) (h - 1))
                (_, (WH w h), pos) = head $ filter (\(name', _, _) -> name' == name) positions


reduceAll' :: [(String, Size, Coords)] -> [(String, Size, Coords)] -> [(String, WH, Pos)]
reduceAll' [] _  = []
reduceAll' (inst:instances) all = solve inst : (reduceAll' instances all)
    where
        solve :: (String, Size, Coords) -> (String, WH, Pos)
        solve (name, (w, h), (x, y)) = (name, 
            WH  (reduceLayoutExpr all w) (reduceLayoutExpr all h), 
            Pos (reduceLayoutExpr all x) (reduceLayoutExpr all y))

reduceAll :: [(String, Size, Coords)] -> [(String, WH, Pos)]
reduceAll all = reduceAll' all all

reduceLayoutExpr :: [(String, Size, Coords)] -> LayoutExpr -> Integer
reduceLayoutExpr instances cexpr = reduce $ reduceCoordsToConsts constantInstances cexpr
    where
        constantInstances = map (\(name, (w, h), (cx, cy)) -> 
            (name, 
                (reduceCoordsToConsts instances w,  reduceCoordsToConsts instances h), 
                (reduceCoordsToConsts instances cx, reduceCoordsToConsts instances cy)))
            instances
        
        reduce :: LayoutExpr -> Integer
        reduce (CAdd ce ce') = reduce ce + reduce ce'
        reduce (CSub ce ce') = reduce ce - reduce ce'
        reduce (CConst c) = c
        reduce expr = error $ "Locations.hs: Coordinate reduction found non-constant value ("++ (show expr) ++ ")"


reduceCoordsToConsts :: [(String, Size, Coords)] -> LayoutExpr -> LayoutExpr
reduceCoordsToConsts instances cexpr = case cexpr of
    (CAdd ce ce') -> (CAdd (reduceCoordsToConsts instances ce) (reduceCoordsToConsts instances ce'))
    (CSub ce ce') -> (CSub (reduceCoordsToConsts instances ce) (reduceCoordsToConsts instances ce'))
    (CX id) -> (\(_, _, (xexpr, _)) -> reduceCoordsToConsts instances xexpr) $ findID instances id
    (CY id) -> (\(_, _, (_, yexpr)) -> reduceCoordsToConsts instances yexpr) $ findID instances id 
    (CWidth id) -> (\(_, (wexpr, _), _) -> reduceCoordsToConsts instances wexpr) $ findID instances id
    (CHeight id) -> (\(_, (_, hexpr), _) -> reduceCoordsToConsts instances hexpr) $ findID instances id
    others -> others


toGraphList :: [(String, Size, Coords)] -> [(Name, [Name])]
toGraphList [] = []
toGraphList ((name, (w, h), (x, y)):rest) = (name, neighbors) : toGraphList rest
    where
        neighbors = references x ++ references y ++ references w ++ references h

        references expr = case expr of
            (CAdd l r) -> references l ++ references r
            (CSub l r) -> references l ++ references r
            (CWidth name) -> [name]
            (CHeight name) -> [name]
            (CX name) -> [name]
            (CY name) -> [name]
            _ -> []

toGraph :: [(Name, [Name])] -> Graph
toGraph graphlist = graph
    where
        (graph, _, _) = graphFromEdges (map (\(n, l) -> (n, n, l)) graphlist)

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
    [] -> error $ "Locations.hs: Could not find ID `" ++ id ++ "` in provided list." ++ show instances


allInstsWithCoords :: System -> [(String, Size, Coords)]
allInstsWithCoords system = 
    map tuplize (sys_elems system) ++
    (concat $ map allInstsWithCoords (sys_subsystems system))
    where
        tuplize elem = (elem_name elem, elem_size elem, elem_coords elem)
        

