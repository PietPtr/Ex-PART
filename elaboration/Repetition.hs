{-# LANGUAGE RecordWildCards #-}
module Repetition where

import Types
import Data.Maybe
import Debug.Trace

unrollRepetition :: RawRepetition -> [Component] -> ([Element] -> [Element])
unrollRepetition rep components = case rep of
    (RawChain _ _ _) -> (\es -> unrollChain es components rep')
    (RawRepeat _ _ _) -> (\es -> unrollRepeat es components rep')
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

unrollRepeat :: [Element] -> [Component] -> Repetition -> [Element]
unrollRepeat elems components rep = map (makeElement elems components rep) [1..(rep_amount rep)]
     

makeElement :: [Element] -> [Component] -> Repetition -> Integer -> Element
makeElement elems components rep i = Element {
        elem_unplaced = False,
        elem_name = repname,
        elem_type = reptype,
        elem_size = repsize,
        elem_coords = repcoords,
        elem_iodefs = repiodefs,
        elem_implementation = case element of
            Nothing -> case component of
                (Just cmp) -> InstanceImpl $ CmpInstance {
                        cins_name = repname,
                        cins_size = repsize,
                        cins_coords = repcoords,
                        cins_cmp = cmp,
                        cins_args = []
                    }
                -- Nothing -> -- this nothing will have fired by now
            (Just elem) -> case elem_implementation elem of
                (InstanceImpl inst) -> case inst of
                    cmpi@CmpInstance{} -> InstanceImpl $ cmpi {
                            cins_name = repname,
                            cins_coords = repcoords,
                            cins_size = repsize -- sincerely hope that nothing later on uses these instead of just elem_coords...
                        }
                    sysi@SysInstance{} -> InstanceImpl $ sysi {
                            sins_name = repname,
                            sins_coords = repcoords,
                            sins_size = repsize
                        }
                (SubsysImpl system) -> SubsysImpl $ system {
                        sys_name = repname,
                        sys_type = reptype,
                        sys_size = repsize,
                        sys_coords = repcoords,
                        sys_elems = map (reformElemCoords elemName repname) (map toElement $ sys_subsystems system)
                            ++ map toElement (sys_instances system)
                    }
    }
    where
        repname = name ++ "_" ++ show i
        repcoords = makeCoords name layout coords i                

        (elemName, args, repsize) = case unIns of
            (UnplacedInstance name args size) -> (name, args, size)

        (name, coords, unIns, layout) = case rep of
            (Repeat name coords unIns _ layout) -> 
                (name, coords, unIns, layout)
            (Chain name coords unIns _ layout _ _) ->
                (name, coords, unIns, layout)

        element = case filter (\e -> (elem_name e) == elemName) elems of
            (x:_) -> Just x
            [] -> Nothing -- 

        component = case filter (\c -> (cmp_type c) == elemName) components of
            (x:_) -> Just x
            [] -> Nothing

        (reptype, repiodefs) = case element of
            (Just e) -> (elem_type e, elem_iodefs e)
            Nothing -> case component of
                (Just c) -> (cmp_type c, catMaybes $ map iso2io $ cmp_isoStats c)
                Nothing -> error $ "Repetition.hs: Cannot find element " ++ elemName ++ " in source files."
        

-- TODO: this code is bug-ridden en was written blindly. With this implementation it is very hard to refer to width/height properties of an unplaced system. But I guess we just "can't" now.
reformElemCoords :: String -> String -> Element -> Element
reformElemCoords en rn elem = elem {
        elem_size = reformCoords' $ elem_size elem,
        elem_coords = reformCoords' $ elem_coords elem,
        elem_implementation = case elem_implementation elem of
            (SubsysImpl impl) -> SubsysImpl $ impl {
                    sys_size = reformCoords' $ sys_size impl,
                    sys_coords = reformCoords' $ sys_coords impl,
                    sys_elems = map (reformElemCoords en rn) (sys_elems impl)
                }
            (InstanceImpl impl) -> InstanceImpl $ case impl of
                cmp@CmpInstance{} -> cmp {
                        cins_size = reformCoords' $ cins_size cmp,
                        cins_coords = reformCoords' $ cins_coords cmp
                    }
                sys@SysInstance{} -> sys {
                        sins_size = reformCoords' $ sins_size sys,
                        sins_coords = reformCoords' $ sins_coords sys
                    }
    }
    where
        reformCoords' = reformCoords en rn



reformCoords :: String -> String -> Coords -> Coords
reformCoords elemname repname (cl, cr) = (reformLayout elemname repname cl, reformLayout elemname repname cr)

reformLayout :: String -> String -> LayoutExpr -> LayoutExpr
reformLayout elemname repname coord = case coord of
    (CAdd cl cr) -> (CAdd (f cl) (f cr))
    (CSub cl cr) -> (CSub (f cl) (f cr))
    (CWidth id) -> (CWidth $ repl id)
    (CHeight id) -> (CHeight $ repl id)
    (CX id) -> (CX $ repl id)
    (CY id) -> (CY $ repl id)
    c -> c
    where
        f = reformLayout elemname repname
        repl id = if id == elemname
            then repname
            else id

makeCoords :: String -> String -> Coords -> Integer -> Coords
makeCoords _ _ coords 1 = coords
makeCoords name layout coords i = case layout of
    "horizontal" -> (CAdd prevX (CWidth n), y) -- TODO (lowprio): add reverse_horizontal and reverse_vertical (reqs more powerful coord exprs)
    "vertical" -> (x, CAdd prevY (CHeight n))
    "identical" -> (x, y)
    _ -> error "Repeat.hs: Unknown layout procedure."
    where
        (x, y) = coords
        n = name ++ "_" ++ show (i - 1)
        prevX = CX n
        prevY = CY n

allChainConnections :: [Repetition] -> [Connection]
allChainConnections rawreps = concat $
    map chainConnections ([ x | x@(Chain {}) <- rawreps])

unrollChain :: [Element] -> [Component] -> Repetition -> [Element]
unrollChain elems components chain = map (makeElement elems components chain) [1..(chn_amount chain)]

chainConnections :: Repetition -> [Connection]
chainConnections chain = map makeConnection [1..(chn_amount - 1)]
    where
        Chain {..} = chain

        makeConnection i = Connection from to
            where
                from = CID (chn_name ++ "_" ++ show i) chn_chainOut
                to = CID (chn_name ++ "_" ++ show (i + 1)) chn_chainIn
                