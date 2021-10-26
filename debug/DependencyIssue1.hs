import Clash.Prelude
import Definitions

demealy :: (() -> i -> ((), o)) -> (i -> o)
demealy f = f'
    where
        f' i = case (f () i) of
            (_, o) -> o

-- split this thing in two?
-- needs to know stuff about dependencies :(((
grouper_o :: () -> (State, State, State) -> ((), (Vec 3 State))
grouper_o () (in1, in2, in3) = ((), (o))
    where
        o = in1:>in2:>in3:>Nil


grouper_c :: () -> (State, State, State) -> ((), (State))
grouper_c () (in1, in2, in3) = ((), (c))
    where
        c = Dead

cell :: (State) -> (State, State, State, State, State, State, State, State) -> ((State), (State))
cell (cell_state) (n, ne, e, se, s, sw, w, nw) = ((cell_state'), (lives))
    where
        lives = cell_state'
    
        neighbor_count = 
            (if n == Alive then 1 else 0) +
            (if ne == Alive then 1 else 0) +
            (if e == Alive then 1 else 0) +
            (if se == Alive then 1 else 0) +
            (if s == Alive then 1 else 0) +
            (if sw == Alive then 1 else 0) +
            (if w == Alive then 1 else 0) +
            (if nw == Alive then 1 else 0)
    
        cell_state' = if cell_state == Alive
            then if (neighbor_count == 2) || (neighbor_count == 3)
                then Alive
                else Dead
            else if neighbor_count == 3
                then Alive
                else Dead

wall :: () -> () -> ((), (State))
wall () () = ((), (c))
    where
        c = Dead

grouper_oM :: HiddenClockResetEnable dom =>
    Signal dom (State, State, State) -> Signal dom (Vec 3 State)
grouper_oM = mealy grouper_o ()

grouper_cM :: HiddenClockResetEnable dom =>
    Signal dom (State, State, State) -> Signal dom (State)
grouper_cM = mealy grouper_c ()

cellM :: HiddenClockResetEnable dom =>
    Signal dom (State, State, State, State, State, State, State, State) -> Signal dom (State)
cellM = mealy cell (Alive)

wallM :: HiddenClockResetEnable dom =>
    Signal dom () -> Signal dom (State)
wallM = mealy wall ()




cells :: HiddenClockResetEnable dom =>
    Signal dom (State, State, State) -> Signal dom (Vec 3 State)
cells input = (grouper_o)
    where
        (this_left_const, this_bot_const, this_top_const) = unbundle input
        (grouper_c) = grouper_cM $ bundle (cell20_lives, cell21_lives, cell22_lives)
        (grouper_o) = grouper_oM $ bundle (cell20_lives, cell21_lives, cell22_lives)
        
        (cell22_lives) = cellM $ bundle (cell21_lives, grouper_c, grouper_c, this_bot_const, this_bot_const, this_bot_const, cell12_lives, cell11_lives)
        (cell21_lives) = cellM $ bundle (cell20_lives, grouper_c, grouper_c, grouper_c, cell22_lives, cell12_lives, cell11_lives, cell10_lives)
        (cell20_lives) = cellM $ bundle (this_top_const, this_top_const, grouper_c, grouper_c, cell21_lives, cell11_lives, cell10_lives, this_top_const)
        (cell12_lives) = cellM $ bundle (cell11_lives, cell21_lives, cell22_lives, this_bot_const, this_bot_const, this_bot_const, cell02_lives, cell01_lives)
        (cell11_lives) = cellM $ bundle (cell10_lives, cell20_lives, cell21_lives, cell22_lives, cell12_lives, cell02_lives, cell01_lives, cell00_lives)
        (cell10_lives) = cellM $ bundle (this_top_const, this_top_const, cell20_lives, cell21_lives, cell11_lives, cell01_lives, cell00_lives, this_top_const)
        (cell02_lives) = cellM $ bundle (cell01_lives, cell11_lives, cell12_lives, this_bot_const, this_bot_const, this_bot_const, this_left_const, this_left_const)
        (cell01_lives) = cellM $ bundle (cell00_lives, cell10_lives, cell11_lives, cell12_lives, cell02_lives, this_left_const, this_left_const, this_left_const)
        (cell00_lives) = cellM $ bundle (this_top_const, this_top_const, cell10_lives, cell11_lives, cell01_lives, this_left_const, this_left_const, this_top_const)


system :: HiddenClockResetEnable dom =>
    Signal dom () -> Signal dom (Vec 3 State)
system input = (cells_result)
    where
        (no_input) = unbundle input
        (wall_bot_c) = wallM (pure ())
        (wall_top_c) = wallM (pure ())
        (wall_left_c) = wallM (pure ())
        (cells_result) = cells $ bundle (wall_left_c, wall_bot_c, wall_top_c)

{-# ANN topEntity
  (Synthesize
    { t_name = "cell"
    , t_inputs = [ PortName "clk", PortName "rst", PortName "en", PortName "input" ]
    , t_output = PortName "output"
    }) #-}

topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System ()
    -> Signal System (Vec 3 State)
topEntity = exposeClockResetEnable system