module Main (main) where

import Geometry as G
import Draw as D
import Graphics.Rendering.Cairo as C hiding (x,y)
import Geometry.Shapes
import qualified Data.Map as M
import  Data.Map (Map)
import Geometry.Coordinates.Hexagonal as Hex
import System.Random.MWC
import Control.Monad.ST

main :: IO ()
main =
    let picWidth = 1000
        picHeight = 1000
    in withSurfaceAuto "out/haskell_logo_circuits.svg" picWidth picHeight (\surface -> renderWith surface mainRender)

lambda :: Polygon
lambda = Polygon
    [ Vec2 113.386719 340.15625
    , Vec2 226.773438 170.078125
    , Vec2 113.386719 0
    , Vec2 198.425781 0
    , Vec2 425.195312 340.15625
    , Vec2 340.15625 340.15625
    , Vec2 269.292969 233.859375
    , Vec2 198.425781 340.15625
    ]

mainRender :: Render ()
mainRender = pure ()

data CellState hex
    = WireFrom hex hex
    | WireEndFrom hex hex

randomAction
    :: HexagonalCoordinate hex
    => GenST s
    -> hex
    -> hex
    -> ST s (CellState hex)
randomAction gen lastStart lastEnd = do
    n <- uniformRM (1, 100) gen
    let _ = n :: Int
    if | n <= 50 -> pure continueStraight
       | n <= 90 -> do
           d <- uniformM gen
           pure (if d then continueRight else continueLeft)
       | otherwise -> pure terminate
  where
    straightOn =lastEnd `hexAdd` hexSubtract lastEnd lastStart
    continueStraight = WireFrom lastEnd straightOn
    continueRight = WireFrom lastEnd (Hex.rotateAround lastEnd 1 straightOn)
    continueLeft = WireFrom lastEnd (Hex.rotateAround lastEnd (-1) straightOn)
    terminate = WireEndFrom lastEnd straightOn

circuitProcess
    :: (Ord hex, HexagonalCoordinate hex)
    => Gen s
    -> Map hex (CellState hex)
    -> hex
    -> hex
    -> ST s (Map hex (CellState hex))
circuitProcess _ knownCells _ _
    | M.size knownCells > 10 = pure knownCells
circuitProcess gen knownCells lastPos currentPos = do
        x <- randomAction gen lastPos currentPos
        case x of
            WireFrom a b -> case M.lookup b knownCells of
                Nothing -> circuitProcess gen (M.insert currentPos x knownCells) a b
                Just _occupied -> circuitProcess gen knownCells lastPos currentPos -- Dirty retry, let’s hope for no infinite loops…
            WireEndFrom a b -> case M.lookup b knownCells of
                Nothing -> circuitProcess gen (M.insert currentPos x knownCells) a b
                Just _occupied -> circuitProcess gen knownCells lastPos currentPos -- Dirty retry, let’s hope for no infinite loops…

findBeginning knownCells =
    let start = goBack (let (_, v) = M.findMin knownCells in v) knownCells
    in _ "continue here :-)"

goBack :: Ord hex => CellState hex -> Map hex (CellState hex) -> hex
goBack (WireFrom from _) knownCells = case M.lookup from knownCells of
    Nothing -> from
    Just before -> goBack before knownCells
goBack (WireEndFrom from _) knownCells = case M.lookup from knownCells of
    Nothing -> from
    Just before -> goBack before knownCells
