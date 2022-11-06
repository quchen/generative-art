module WaveFunctionCollapse where

type Plane a = [[a]]

data WfcSettings a = WfcSettings
    { wfcWidth :: Int
    , wfcHeight :: Int
    , wfcTiles :: [a]
    , wfcLegalAdjacency :: Adjacency a -> Bool
    }

data Adjacency a
    = a `Above` a
    | a `LeftOf` a
    | a `Below` a
    | a `RightOf` a

type Coordinate = (Int, Int)

wfc :: Eq a => WfcSettings a -> Maybe (Plane a)
wfc WfcSettings{..} = go initialPlane
  where
    initialPlane = replicate wfcHeight (replicate wfcWidth wfcTiles)
    go plane = case findMin plane of
        Just minimumElement -> go (propagate minimumElement (collapse minimumElement plane))
        Nothing -> Just (fmap (fmap head) plane)

propagate :: Coordinate -> Plane [a] -> Plane [a]
propagate = undefined

collapse :: Coordinate -> Plane [a] -> Plane [a]
collapse = undefined

findMin :: Plane [a] -> Maybe Coordinate
findMin = undefined
