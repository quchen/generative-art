module Circuits.ReconstructWires (reconstructWires) where



import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import Circuits.GrowingProcess
import Geometry.Coordinates.Hexagonal as Hex



reconstructSingleWire :: Map Hex CellState -> Hex -> [Hex]
reconstructSingleWire cellMap start = start : case M.lookup start cellMap of
    Nothing -> error "Reached end of wire unexpectedly"
    Just (WireTo next) -> reconstructSingleWire cellMap next
    Just WireEnd -> []

-- | Convert the (rather abstract) result of the circuit growing process into a set
-- of hex-polylines, which are much simpler to work with.
reconstructWires :: Circuits -> Set [Hex]
reconstructWires circuits = S.map (reconstructSingleWire (_nodes circuits)) (_starts circuits)
