{-# LANGUAGE RecordWildCards #-}
module Repeat where

import Types


unrollRepeat :: [Component] -> Repetition -> [Instance]
unrollRepeat cmps rep = map (makeInstance cmps rep) [1..(rep_amount rep)]
     

makeInstance :: [Component] -> Repetition -> Integer -> Instance
makeInstance cmps rep i = Instance {
        ins_name = name ++ "_" ++ show i,
        ins_cmp = component,
        ins_args = args,
        ins_size = inst_size,
        ins_coords = makeCoords name layout coords i
    }
    where
        (component_name, args, inst_size) = case unIns of
            (UnplacedInstance name args size) -> (name, args, size)

        (name, coords, unIns, layout) = case rep of
            (Repeat name coords unIns _ layout) -> 
                (name, coords, unIns, layout)
            (Chain name coords unIns _ layout _ _) ->
                (name, coords, unIns, layout)

        component = case filter (\c -> (cmp_name c) == component_name) cmps of
            (x:_) -> x
            [] -> error $ "Repeat.hs: Cannot find component " ++ component_name ++ " in .expc file."


makeCoords :: String -> String -> Coords -> Integer -> Coords
makeCoords name layout coords 1 = coords
makeCoords name layout coords i = case layout of
    "horizontal" -> (CAdd prevX (CWidth n), y) -- TODO (feature): add reverse_horizontal and reverse_vertical (reqs more powerful coord exprs)
    "vertical" -> (x, CAdd prevY (CHeight n))
    _ -> error "Repeat.hs: Unknown layout procedure."
    where
        (x, y) = coords
        n = name ++ "_" ++ show (i - 1)
        prevX = CX n
        prevY = CY n


