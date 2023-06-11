module Geometry.Algorithms.Clipping.CohenSutherland (cohenSutherland) where



import Data.Bits
import Geometry.Core
import Util



-- $setup
-- >>> import Geometry.Algorithms.Sampling
-- >>> import Control.Monad
-- >>> import Control.Monad.ST
-- >>> import Draw
-- >>> import Data.Maybe
-- >>> import Data.Traversable
-- >>> import qualified Graphics.Rendering.Cairo as C
-- >>> import qualified System.Random.MWC as MWC


-- | Constrain a line to the inside of a box with the Cohen-Sutherland clipping algorithm.
--
-- <<docs/haddock/Geometry/Algorithms/Clipping/CohenSutherland/cohenSutherland.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Clipping/CohenSutherland/cohenSutherland.svg" 300 200 $ \_ -> do
--     let viewport = boundingBox [zero, Vec2 300 200]
--         mask = shrinkBoundingBox 20 viewport
--         lines = runST $ do
--             gen <- MWC.create
--             ps <- poissonDisc gen mask 20 4
--             for ps $ \p -> do
--                 angle <- MWC.uniformM gen
--                 let makeLooong = resizeLineSymmetric (const 500)
--                 pure (makeLooong (Line p (p +. polar angle 1)))
--         clippedLines = mapMaybe (cohenSutherland mask) lines
--     for_ clippedLines $ \line -> cairoScope $ do
--         setColor (mma 1)
--         sketch line
--         C.stroke
--     cairoScope $ do
--         C.setLineWidth 2
--         setColor (mma 0)
--         sketch (boundingBoxPolygon mask)
--         C.stroke
-- :}
-- Generated file: size 22KB, crc32: 0xa5dc883f
cohenSutherland :: BoundingBox -> Line -> Maybe Line
cohenSutherland bb = \line -> let Line start end = line in loop line (outCode start) (outCode end)
  where

    BoundingBox (Vec2 xMin yMin) (Vec2 xMax yMax) = bb

    -- Translated from Wikipedia’s pseudocode
    bit_LEFT   = bit 1 :: Int
    bit_RIGHT  = bit 2
    bit_BOTTOM = bit 3
    bit_TOP    = bit 4

    outCode (Vec2 x y) =
        let codeX | x < xMin = bit_LEFT
                  | x > xMax = bit_RIGHT
                  | otherwise = 0
            codeY | y < yMin = bit_TOP
                  | y > yMax = bit_BOTTOM
                  | otherwise = 0
        in codeX .|. codeY

    loop line@(Line p0@(Vec2 x0 y0) p1@(Vec2 x1 y1)) outcode0 outcode1

        -- Both points are inside the window, no clipping necessary
        | outcode0 .|. outcode1 == 0 = Just line

        -- Both points share an outside region of the window, so the line is not
        -- inside the bounding box at all.
        | outcode0 .&. outcode1 /= 0 = Nothing

        -- Clipping necessary
        | otherwise =
            let outcode' = max outcode0 outcode1
                p | 0 /= outcode' .&. bit_BOTTOM = Vec2 (x0 + (x1-x0) * (yMax-y0) / (y1-y0)) yMax
                  | 0 /= outcode' .&. bit_TOP    = Vec2 (x0 + (x1-x0) * (yMin-y0) / (y1-y0)) yMin
                  | 0 /= outcode' .&. bit_RIGHT  = Vec2 xMax (y0 + (y1-y0) * (xMax-x0) / (x1-x0))
                  | 0 /= outcode' .&. bit_LEFT   = Vec2 xMin (y0 + (y1-y0) * (xMin-x0) / (x1-x0))
                  | otherwise = bugError "cohenSutherland" "Impossible!"

            in if outcode' == outcode0
                then loop (Line p p1) (outCode p) outcode1
                else loop (Line p0 p) outcode0 (outCode p)
