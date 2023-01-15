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
import Geometry                     as G hiding (Grid)
import Geometry.Algorithms.WaveFunctionCollapse
import Data.Function (on)
import Data.Maybe (maybeToList)



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

scaleFactor :: Double
scaleFactor = 1

main :: IO ()
main = pure ()

drawGrid :: Double -> Grid (Tile ColorIndex) -> C.Render ()
drawGrid cellSize grid@(Grid _ _ _ _ cells) = do
    let (w, h) = size grid
    for_ [0..w-1] $ \x -> for_ [0..h-1] $ \y -> cairoScope $ do
        C.translate (fromIntegral x * cellSize) (fromIntegral y * cellSize)
        drawCell cellSize (cells V.! y V.! x)

drawCell :: Double -> Tile ColorIndex -> C.Render ()
drawCell cellSize = cairoScope . \case
    Empty -> pure ()
    Single Horizontal i -> do
        sketch (Line (Vec2 (-cellSize/2) 0) (Vec2 (cellSize/2) 0))
        setColor (mathematica97 i)
        C.stroke
    Single Vertical i -> do
        sketch (Line (Vec2 0 (-cellSize/2)) (Vec2 0 (cellSize/2)))
        setColor (mathematica97 i)
        C.stroke
    Cross Vertical i j -> do
        drawCell cellSize (Single Horizontal i)
        drawCell cellSize (Single Vertical j)
    Cross Horizontal i j -> do
        drawCell cellSize (Single Vertical j)
        drawCell cellSize (Single Horizontal i)

wfcSettings :: WfcSettings (Tile ColorIndex)
wfcSettings = WfcSettings {..}
  where
    wfcTiles = M.fromList [Empty, Single Horizontal 0, Single Vertical 0, Cross Horizontal 0 0, Cross Vertical 0 0]
    wfcLocalProjection grid = M.map fst $ M.filter (any compatible . snd) $ M.map (\tile -> (tile, neighbours tile)) (extract grid)
      where
        neighbours tile = 
            [ Stencil3x3 Nothing a Nothing b tile c Nothing d Nothing
            | a <- traverse (M.toList . extract) (up    grid)
            , b <- traverse (M.toList . extract) (left  grid)
            , c <- traverse (M.toList . extract) (right grid)
            , d <- traverse (M.toList . extract) (down  grid)
            ]

data Tile a
    = Empty
    | Single Direction a
    | Cross Direction a a
    -- ^ Direction = which one is on top
    -- Tags: Horizontal, Vertical
    deriving (Eq, Ord, Show)

data Direction = Horizontal | Vertical deriving (Eq, Ord, Show)

type ColorIndex = Int

component :: Direction -> Tile a -> Tile a
component Horizontal (Single Horizontal a) = Single Horizontal a
component Horizontal (Cross Horizontal a b) = Single Horizontal a
component Horizontal (Cross Vertical a b) = Single Horizontal a
component Horizontal _ = Empty
component Vertical (Single Vertical a) = Single Vertical a
component Vertical (Cross Horizontal a b) = Single Horizontal b
component Vertical (Cross Vertical a b) = Single Horizontal b
component Vertical _ = Empty


compatible :: Eq a => Stencil3x3 (Tile a) -> Bool
compatible (Stencil3x3 _ a _ b Empty c _ d _)
    | Just Single{} <- component Vertical <$> a = False
    | Just Single{} <- component Horizontal <$> b = False
    | Just Single{} <- component Horizontal <$> c = False
    | Just Single{} <- component Vertical <$> d = False
    | otherwise = True
compatible (Stencil3x3 _ a _ b (Single Horizontal color) c _ d _)
    | Just Single{} <- component Vertical <$> a = False
    | Just Single{} <- component Vertical <$> d = False
    | Just (Single Horizontal color1) <- component Horizontal <$> b
    , Just (Single Horizontal color2) <- component Horizontal <$> c 
    = color == color1 && color == color2
    | otherwise = False
compatible (Stencil3x3 _ a _ b (Single Vertical color) c _ d _)
    | Just Single{} <- component Horizontal <$> b = False
    | Just Single{} <- component Horizontal <$> c = False
    | Just (Single Vertical color1) <- component Vertical <$> a
    , Just (Single Vertical color2) <- component Vertical <$> d
    = color == color1 && color == color2
    | otherwise = False
compatible (Stencil3x3 _ a _ b (Cross _ colorH colorV) c _ d _)
    | Just (Single Vertical colorA) <- component Vertical <$> a
    , Just (Single Horizontal colorB) <- component Horizontal <$> b
    , Just (Single Horizontal colorC) <- component Horizontal <$> c
    , Just (Single Vertical colorD) <- component Vertical <$> d
    = colorA == colorV && colorB == colorH && colorC == colorH && colorD == colorV
    | otherwise = False

extractStencil :: Stencil3x3 a -> a 
extractStencil (Stencil3x3 _ _ _ _ e _ _ _ _) = e
