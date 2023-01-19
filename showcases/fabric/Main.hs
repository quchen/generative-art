{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where



import           Data.List
import qualified Data.MultiSet as M
import           Data.Ord
import           Graphics.Rendering.Cairo as C hiding (height, width, x, y)
import           Prelude                  hiding ((**))
import           System.Random.MWC
import qualified Data.Vector as V

import Draw hiding (Cross)
import Draw.Grid
import Geometry                     as G hiding (Grid)
import Geometry.Algorithms.WaveFunctionCollapse
import Control.Monad.ST (runST)



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

cellSize :: Num a => a
cellSize = 20

main :: IO ()
main = do
    let grid = (\[a] -> a) . M.toList <$> last gridGenerations
    render "out/fabric.png" picWidth picHeight $ do
        cairoScope (setColor white >> paint)
        renderGrid cellSize grid

instance DrawToSize Tile where
    drawToSize (w, h) = drawCell (min w h)

gridGenerations :: [Grid (M.MultiSet Tile)]
gridGenerations = runST $ do
    gen <- create
    wfc wfcSettings (picWidth `div` cellSize) (picHeight `div` cellSize) gen

renderGrid :: Double -> Grid Tile -> C.Render ()
renderGrid cellSize grid@(Grid _ _ _ _ cells) = do
    let (w, h) = size grid
    for_ [0..w-1] $ \x -> for_ [0..h-1] $ \y -> cairoScope $ do
        C.translate (fromIntegral x * cellSize) (fromIntegral y * cellSize)
        setLineWidth (cellSize/2)
        drawCell cellSize (cells V.! y V.! x)

drawCell :: Double -> Tile -> C.Render ()
drawCell cellSize = cairoScope . \case
    Empty -> pure ()
    Single Horizontal i -> do
        sketch (Line (Vec2 0 (cellSize/2)) (Vec2 cellSize (cellSize/2)))
        setColor (mathematica97 i)
        C.stroke
    Single Vertical i -> do
        sketch (Line (Vec2 (cellSize/2) 0) (Vec2 (cellSize/2) cellSize))
        setColor (mathematica97 i)
        C.stroke
    Cross Vertical i j -> do
        sketch (Line (Vec2 0 (cellSize/2)) (Vec2 cellSize (cellSize/2)))
        setColor (mathematica97 i)
        C.stroke
        sketch (Line (Vec2 (cellSize/2) 0               ) (Vec2 (cellSize/2) (0.2 * cellSize)))
        sketch (Line (Vec2 (cellSize/2) (0.8 * cellSize)) (Vec2 (cellSize/2) cellSize))
        setColor (mathematica97 j)
        C.stroke
    Cross Horizontal i j -> do
        sketch (Line (Vec2 0                (cellSize/2)) (Vec2 (0.2 * cellSize) (cellSize/2)))
        sketch (Line (Vec2 (0.8 * cellSize) (cellSize/2)) (Vec2 cellSize         (cellSize/2)))
        setColor (mathematica97 i)
        C.stroke
        sketch (Line (Vec2 (cellSize/2) 0) (Vec2 (cellSize/2) cellSize))
        setColor (mathematica97 j)
        C.stroke

wfcSettings :: WfcSettings Tile
wfcSettings = WfcSettings {..}
  where
    wfcTiles = M.fromList $ concat
        [ [Empty]
        , Single Horizontal <$> colors
        , Single Vertical <$> colors
        , Cross Horizontal <$> colors <*> colors
        , Cross Vertical <$> colors <*> colors
        ]
      where colors = [0..3]
    wfcLocalProjection :: Grid (Touched (M.MultiSet Tile)) -> Touched (M.MultiSet Tile)
    wfcLocalProjection grid
        | isUntouched neighboursLeft && isUntouched neighboursDown && isUntouched neighboursRight && isUntouched neighboursUp
        = Untouched oldState
        | newState == oldState
        = Untouched oldState
        | otherwise
        = Touched newState
      where
        neighboursLeft  = fmap (fmap M.toList . extract) (left  grid)
        neighboursDown  = fmap (fmap M.toList . extract) (down  grid)
        neighboursRight = fmap (fmap M.toList . extract) (right grid)
        neighboursUp    = fmap (fmap M.toList . extract) (up    grid)
        oldState = getTouched (extract grid)
        newState = M.fromAscOccurList
            [ (tile, n)
            | (tile, n) <- M.toAscOccurList oldState
            , maybe True (any (`beside` tile) . getTouched) neighboursLeft
            , maybe True (any (tile `above`)  . getTouched)  neighboursDown
            , maybe True (any (tile `beside`) . getTouched) neighboursRight
            , maybe True (any (`above` tile)  . getTouched)  neighboursUp
            ]
        isUntouched = \case
            Nothing -> True
            Just (Untouched _) -> True
            _otherwise -> False


data Tile
    = Empty
    | Single Direction ColorIndex
    | Cross Direction ColorIndex ColorIndex
    -- ^ Direction = which one is on top
    -- Tags: Horizontal, Vertical
    deriving (Eq, Ord, Show)

data Direction = Horizontal | Vertical deriving (Eq, Ord, Show)

type ColorIndex = Int

component :: Direction -> Tile -> Tile
component Horizontal (Single Horizontal a) = Single Horizontal a
component Horizontal (Cross Horizontal a b) = Single Horizontal a
component Horizontal (Cross Vertical a b) = Single Horizontal a
component Horizontal _ = Empty
component Vertical (Single Vertical a) = Single Vertical a
component Vertical (Cross Horizontal a b) = Single Vertical b
component Vertical (Cross Vertical a b) = Single Vertical b
component Vertical _ = Empty

beside, above :: Tile -> Tile -> Bool
l `beside` r
    | Empty <- component Horizontal l
    , Empty <- component Horizontal r
    = True
    | Single Horizontal a <- component Horizontal l
    , Single Horizontal b <- component Horizontal r
    = a == b
    | otherwise
    = False

t `above` b
    | Empty <- component Vertical t
    , Empty <- component Vertical b
    = True
    | Single Vertical a <- component Vertical t
    , Single Vertical b <- component Vertical b
    = a == b
    | otherwise
    = False
