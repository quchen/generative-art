module Mondrian where

import Control.Monad (guard, when)
import Control.Monad.Trans.State (get, modify', StateT, evalStateT, gets, mapStateT, execState, State)
import Data.Map as Map
import Data.Maybe (maybeToList, catMaybes, isJust)
import Prelude hiding (Either(..))

import Geometry ( Polygon(..), Vec2(..) )
import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)



type Mondrian = Map Vertex NextVertex

data Direction = Down | Right | Up | Left deriving (Eq, Ord, Show)

turn :: Direction -> Direction
turn Down = Right
turn Right = Up
turn Up = Left
turn Left = Down

type Vertex = (Int, Int)

type Edge = (Vertex, Direction)

type NextVertex = Map Direction Vertex

mondrianBaseGrid :: Int -> Int -> Mondrian
mondrianBaseGrid width height = Map.fromList $ do
    x <- [0..width]
    y <- [0..height]
    let nextVertex = Map.fromList $ catMaybes
            [ if x < width  then Just (Right, (x+1, y)) else Nothing
            , if y > 0      then Just (Up,    (x, y-1)) else Nothing
            , if x > 0      then Just (Left,  (x-1, y)) else Nothing
            , if y < height then Just (Down,  (x, y+1)) else Nothing ]
    pure ((x, y), nextVertex)

asPolygons :: Double -> Mondrian -> [Polygon]
asPolygons gridSize mondrian = flip evalStateT mondrian $ do
    vertices <- gets keys
    vertex <- lift vertices
    guard (isUpperLeft vertex)
    toPolygon <$> mapStateT maybeToList (constructPolygon vertex)
  where
    isUpperLeft :: Vertex -> Bool
    isUpperLeft vertex =
        let nextVertices = mondrian !? vertex
            rightEdge = nextVertices >>= (!? Right)
            downEdge  = nextVertices >>= (!? Down)
        in  isJust rightEdge && isJust downEdge
    toPolygon :: [Vertex] -> Polygon
    toPolygon = Polygon . fmap (\(x, y) -> Vec2 (gridSize * fromIntegral x) (gridSize * fromIntegral y))

constructPolygon :: Vertex -> StateT Mondrian Maybe [Vertex]
constructPolygon startVertex = (startVertex :) <$> (go Down =<< nextVertex Down startVertex)
  where
    go :: Direction -> Vertex -> StateT Mondrian Maybe [Vertex]
    go direction vertex
        | vertex == startVertex = pure []
        | otherwise = do
            let nextDirection = turn direction
                goInNextDirection = do
                    vertex' <- nextVertex nextDirection vertex
                    modify' (unsafeRemoveEdge (vertex, nextDirection))
                    go (turn direction) vertex'
                goInSameDirection = do
                    vertex' <- nextVertex direction vertex
                    modify' (unsafeRemoveEdge (vertex, direction))
                    go direction vertex'
            (vertex :) <$> goInNextDirection <|> goInSameDirection

    nextVertex :: Direction -> Vertex -> StateT Mondrian Maybe Vertex
    nextVertex direction vertex = do
        mondrian <- get
        lift (mondrian !? vertex >>= (!? direction))

unsafeRemoveEdge :: Edge -> Mondrian -> Mondrian
unsafeRemoveEdge (vertex, direction) = Map.adjust (Map.delete direction) vertex

removeEdge :: Edge -> Mondrian -> Mondrian
removeEdge edge@(vertex, _) mondrian = flip execState mondrian $ when (hasEdge edge mondrian) $ do
    edge'@(vertex', _) <- gets (edgeTwin edge)
    modify' $ unsafeRemoveEdge edge
    modify' $ unsafeRemoveEdge edge'
    cleanup vertex
    cleanup vertex'
  where
    cleanup :: Vertex -> State Mondrian ()
    cleanup vertex = do
        remainingEdges <- gets (Map.toList . assumeJust "cleanup: vertex exists" . (!? vertex))
        case remainingEdges of
            [] -> modify' (Map.delete vertex)
            -- Vertex with only a single edge cannot exist
            [(d, v')] -> do
                modify' (unsafeRemoveEdge (v', opposite d))
                modify' (delete vertex)
                cleanup v'
            -- Vertex with two edges that are not opposite, i.e. corners, cannot exist as a result from removing edges
            [(d1, v1'), (d2, v2')] | d1 /= opposite d2 -> do
                modify' (unsafeRemoveEdge (v1', opposite d1))
                modify' (unsafeRemoveEdge (v2', opposite d2))
                modify' (delete vertex)
                cleanup v1' >> cleanup v2'
            _ -> pure ()

hasEdge :: Edge -> Mondrian -> Bool
hasEdge (vertex, direction) mondrian = isJust (mondrian !? vertex >>= (!? direction))

edgeTwin :: Edge -> Mondrian -> Edge
edgeTwin (vertex, direction) mondrian = (assumeJust "edgeTwin: direction exists" (assumeJust "edgeTwin: Vertex exists" (mondrian !? vertex) !? direction), opposite direction)

opposite :: Direction -> Direction
opposite = turn . turn

assumeJust :: String -> Maybe a -> a
assumeJust msg Nothing = error msg
assumeJust _ (Just a) = a
