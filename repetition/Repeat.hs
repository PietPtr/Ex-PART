{-# LANGUAGE RecordWildCards #-}
module Repeat where

import Types



unrollRepeat :: [Component] -> Repetition -> [Instance]
unrollRepeat cmps rep = map makeInstance [0..(rep_amount - 1)]
    where
        Repeat {..} = rep -- fails on Chain call right?

        (component_name, args, inst_size) = case rep_unplacedInstance of
            (UnplacedInstance name args size) -> (name, args, size)

        component = case filter (\c -> (cmp_name c) == component_name) cmps of
            (x:_) -> x
            [] -> error $ "Cannot find component " ++ component_name ++ " in .expc file."

        makeInstance i = Instance {
                ins_name = rep_name ++ "_" ++ show i,
                ins_cmp = component,
                ins_args = args,
                ins_size = inst_size,
                ins_coords = makeCoords i
            }
        
        makeCoords 0 = rep_coords
        makeCoords i = case rep_layout of
            "horizontal" -> (CAdd prevX (CWidth n), y)
            "vertical" -> (x, CAdd prevY (CHeight n))
            _ -> error "Unknown layout procedure."
            where
                (x, y) = rep_coords
                n = rep_name ++ "_" ++ show (i - 1)
                prevX = CX n
                prevY = CY n



{-
Repeat 
    "slowCounters" 
    (CConst 0,CConst 5) 
    [Comp (UnplacedInstance "counter" [] (2,2)),
     Amount 6,
     Layout "horizontal"
    ]
-}