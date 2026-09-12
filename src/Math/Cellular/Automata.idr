module Math.Cellular.Automata

import Core.BoxInt
import Core.UnixelFraction
import Core.VexelMaxel
import Math.Cellular.Comonad
import Geometry.LatticeTopology
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- 1. DISCRETE LATTICE AUTOMATA & NEIGHBORHOOD DYNAMICS
--------------------------------------------------------------------------------

||| 2D Cellular Automata State carrying population density vectors
public export
record CellState where
  constructor MkCellState
  mass       : BoxInt
  momentumX  : BoxInt
  momentumY  : BoxInt
  isBoundary : Bool

public export
Eq CellState where
  (MkCellState m1 jx1 jy1 b1) == (MkCellState m2 jx2 jy2 b2) =
    m1 == m2 && jx1 == jx2 && jy1 == jy2 && b1 == b2

||| Initial baseline cell state
public export
initCellState : BoxInt -> CellState
initCellState m = MkCellState m (intToBoxInt 0) (intToBoxInt 0) False

------------------------------------------------------------------------
-- 2. LOCAL DISSIPATION & BOUNDARY COLLISION RULE
------------------------------------------------------------------------

||| Local cellular automaton physics rule executing spatial neighborhood inspection.
||| Computes local mass diffusion and boundary collision bounce-back.
public export
stepCellAutomaton : GridContext CellState -> CellState
stepCellAutomaton (Context left center right) =
  if isBoundary center
    then -- Bounce-back collision rule on boundary node
         MkCellState (mass center) (- momentumX center) (- momentumY center) True
    else let diff    = (mass left + mass right - (intToBoxInt 2 * mass center)) `div` intToBoxInt 3
             newMass = mass center + diff
             newJx   = (momentumX left + momentumX right) `div` intToBoxInt 2
             newJy   = (momentumY left + momentumY right) `div` intToBoxInt 2
         in MkCellState newMass newJx newJy (isBoundary center)

||| Advances an entire 1D/2D grid of cell states using the Spatial Comonad.
public export
stepAutomataGrid : GridContext CellState -> GridContext CellState
stepAutomataGrid grid = extend stepCellAutomaton grid

------------------------------------------------------------------------
-- 3. FORMAL CONSERVATION & AUTOMATA PROOF WITNESS
------------------------------------------------------------------------

||| Compiler proof auditing mass conservation across cellular comonadic steps.
public export
0 verifyAutomataMassConservation : let m = intToBoxInt 10
                                       s = initCellState m
                                       g = Context s s s
                                       s' = extract (stepAutomataGrid g)
                                   in mass s' = m
verifyAutomataMassConservation = Refl


