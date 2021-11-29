{-# LANGUAGE RecordWildCards #-}
module Repetition where

import Types

unrollRepetition :: RawRepetition -> ([Element] -> [Element])
unrollRepetition rep = case rep of
    (RawChain _ _ _) -> (\es -> unrollChain es rep')
    (RawRepeat _ _ _) -> (\es -> unrollRepeat es rep')
    where
        rep' = fitRepetition rep


fitRepetition :: RawRepetition -> Repetition
fitRepetition rep = repetition
    where
        repetition = case rep of
            (RawRepeat name coords options) -> Repeat {
                    rep_name = name,
                    rep_coords = coords,
                    rep_unplacedInstance = unplacedInstance options,
                    rep_amount = amount options,
                    rep_layout = layout options
                }
            (RawChain name coords options) -> Chain {
                    chn_name = name,
                    chn_coords = coords,
                    chn_unplacedInstance = unplacedInstance options,
                    chn_amount = amount options,
                    chn_layout = layout options,
                    chn_chainIn = case [ x | ChainIn x <- options ] of
                        [] -> error "Parse_expi.hs: Missing option `chain_in` in chain statement"
                        (x:_) -> x,
                    chn_chainOut = case [ x | ChainOut x <- options ] of
                        [] -> error "Parse_expi.hs: Missing option `chain_out` in chain statement"
                        (x:_) -> x
            }

        unplacedInstance options = case [ x | Comp x <- options ] of
            [] -> error "Parse_expi.hs: Missing option `component` in repetition statement."
            (x:_) -> x
        amount options = case [ x | Amount x <- options ] of
            [] -> error "Parse_expi.hs: Missing option `amount` in a repetition statement."
            (x:_) -> x
        layout options = case [x | Layout x <- options ] of
            [] -> error "Parse_expi.hs: Missing option `layout` in a repetition statement."
            (x:_) -> x

unrollRepeat :: [Element] -> Repetition -> [Element]
unrollRepeat elems rep = map (makeElement elems rep) [1..(rep_amount rep)]
     

makeElement :: [Element] -> Repetition -> Integer -> Element
makeElement elems rep i = element {
        elem_name = name ++ "_" ++ show i,
        elem_type = elem_type element,
        elem_size = inst_size,
        elem_coords = makeCoords name layout coords i,
        elem_iodefs = elem_iodefs element,
        elem_implementation = elem_implementation element
    }
    where
        (elemName, args, inst_size) = case unIns of
            (UnplacedInstance name args size) -> (name, args, size)

        (name, coords, unIns, layout) = case rep of
            (Repeat name coords unIns _ layout) -> 
                (name, coords, unIns, layout)
            (Chain name coords unIns _ layout _ _) ->
                (name, coords, unIns, layout)

        element = case filter (\e -> (elem_name e) == elemName) elems of
            (x:_) -> x
            [] -> error $ "Repeat.hs: Cannot find element " ++ elemName ++ " in source files: " ++ show (map elem_name elems)


makeCoords :: String -> String -> Coords -> Integer -> Coords
makeCoords _ _ coords 1 = coords
makeCoords name layout coords i = case layout of
    "horizontal" -> (CAdd prevX (CWidth n), y) -- TODO (lowprio): add reverse_horizontal and reverse_vertical (reqs more powerful coord exprs)
    "vertical" -> (x, CAdd prevY (CHeight n))
    _ -> error "Repeat.hs: Unknown layout procedure."
    where
        (x, y) = coords
        n = name ++ "_" ++ show (i - 1)
        prevX = CX n
        prevY = CY n

allChainConnections :: [Repetition] -> [Connection]
allChainConnections rawreps = concat $
    map chainConnections ([ x | x@(Chain {}) <- rawreps])

unrollChain :: [Element] -> Repetition -> [Element]
unrollChain elems chain = map (makeElement elems chain) [1..(chn_amount chain)]

chainConnections :: Repetition -> [Connection]
chainConnections chain = map makeConnection [1..(chn_amount - 1)]
    where
        Chain {..} = chain

        makeConnection i = Connection from to
            where
                from = CID (chn_name ++ "_" ++ show i) chn_chainOut
                to = CID (chn_name ++ "_" ++ show (i + 1)) chn_chainIn
                