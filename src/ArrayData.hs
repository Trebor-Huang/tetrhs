{-# LANGUAGE FlexibleContexts #-}
module ArrayData where

import Board
import Data.Array.IArray

srs :: IArray a (Int, Int) => KickTable a
srs = listArray ((minBound,0,0,0), (maxBound,3,3,4)) [
        -- empty Unused
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        -- ZSJLT
        (0,0),(0,0),(0,0),(0,0),(0,0), -- 0 -> 0 Unused
        (0,0),(-1,0),(-1,1),(0,-2),(-1,-2), -- 0 -> 1
        (0,0),(0,0),(0,0),(0,0),(0,0), -- 0 -> 2 Not supported
        (0,0),(1,0),(1,1),(0,-2),(1,-2), -- 0 -> 3
        (0,0),(1,0),(1,-1),(0,2),(1,2), -- 1 -> 0
        (0,0),(0,0),(0,0),(0,0),(0,0), -- 1 -> 1
        (0,0),(1,0),(1,-1),(0,2),(1,2), -- 1 -> 2
        (0,0),(0,0),(0,0),(0,0),(0,0), -- 1 -> 3
        (0,0),(0,0),(0,0),(0,0),(0,0), -- 2 -> 0
        (0,0),(-1,0),(-1,1),(0,-2),(-1,-2), -- 2 -> 1
        (0,0),(0,0),(0,0),(0,0),(0,0), -- 2 -> 2
        (0,0),(1,0),(1,1),(0,-2),(1,-2), -- 2 -> 3
        (0,0),(-1,0),(-1,-1),(0,2),(-1,2), -- 3 -> 0
        (0,0),(0,0),(0,0),(0,0),(0,0), -- 3 -> 1
        (0,0),(-1,0),(-1,-1),(0,2),(-1,2), -- 3 -> 2
        (0,0),(0,0),(0,0),(0,0),(0,0), -- 3 -> 3
        -- S Repeats
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(-1,0),(-1,1),(0,-2),(-1,-2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(1,0),(1,1),(0,-2),(1,-2),
        (0,0),(1,0),(1,-1),(0,2),(1,2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(1,0),(1,-1),(0,2),(1,2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(-1,0),(-1,1),(0,-2),(-1,-2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(1,0),(1,1),(0,-2),(1,-2),
        (0,0),(-1,0),(-1,-1),(0,2),(-1,2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(-1,0),(-1,-1),(0,2),(-1,2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        -- J
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(-1,0),(-1,1),(0,-2),(-1,-2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(1,0),(1,1),(0,-2),(1,-2),
        (0,0),(1,0),(1,-1),(0,2),(1,2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(1,0),(1,-1),(0,2),(1,2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(-1,0),(-1,1),(0,-2),(-1,-2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(1,0),(1,1),(0,-2),(1,-2),
        (0,0),(-1,0),(-1,-1),(0,2),(-1,2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(-1,0),(-1,-1),(0,2),(-1,2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        -- L
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(-1,0),(-1,1),(0,-2),(-1,-2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(1,0),(1,1),(0,-2),(1,-2),
        (0,0),(1,0),(1,-1),(0,2),(1,2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(1,0),(1,-1),(0,2),(1,2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(-1,0),(-1,1),(0,-2),(-1,-2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(1,0),(1,1),(0,-2),(1,-2),
        (0,0),(-1,0),(-1,-1),(0,2),(-1,2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(-1,0),(-1,-1),(0,2),(-1,2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        -- T
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(-1,0),(-1,1),(0,-2),(-1,-2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(1,0),(1,1),(0,-2),(1,-2),
        (0,0),(1,0),(1,-1),(0,2),(1,2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(1,0),(1,-1),(0,2),(1,2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(-1,0),(-1,1),(0,-2),(-1,-2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(1,0),(1,1),(0,-2),(1,-2),
        (0,0),(-1,0),(-1,-1),(0,2),(-1,2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(-1,0),(-1,-1),(0,2),(-1,2),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        -- O unused
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        (0,0),(0,0),(0,0),(0,0),(0,0),
        -- I
        (0,0),(0,0),(0,0),(0,0),(0,0), -- 0 -> 0
        (0,0),(-2,0),(1,0),(-2,-1),(1,2), -- 0 -> 1
        (0,0),(0,0),(0,0),(0,0),(0,0), -- 0 -> 2
        (0,0),(-1,0),(2,0),(-1,2),(2,-1), -- 0 -> 3
        (0,0),(2,0),(1,0),(2,1),(-1,-2), -- 1 -> 0
        (0,0),(0,0),(0,0),(0,0),(0,0), -- 1 -> 1
        (0,0),(-1,0),(2,0),(-1,2),(2,-1), -- 1 -> 2
        (0,0),(0,0),(0,0),(0,0),(0,0), -- 1 -> 3
        (0,0),(0,0),(0,0),(0,0),(0,0), -- 2 -> 0
        (0,0),(1,0),(-2,0),(1,-2),(-2,1), -- 2 -> 1
        (0,0),(0,0),(0,0),(0,0),(0,0), -- 2 -> 2
        (0,0),(2,0),(-1,0),(2,1),(-1,-2), -- 2 -> 3
        (0,0),(1,0),(-2,0),(1,-2),(-2,1), -- 3 -> 0
        (0,0),(0,0),(0,0),(0,0),(0,0), -- 3 -> 1
        (0,0),(-2,0),(1,0),(-2,-1),(1,2), -- 3 -> 2
        (0,0),(0,0),(0,0),(0,0),(0,0) -- 3 -> 3
    ]

guideLineSpawnPositions :: Array Piece Position
guideLineSpawnPositions = listArray (minBound, maxBound)
    [(0,0), (3,19), (3,19), (3,19), (3,19), (3,19), (4,20), (3,18)] -- xZSJLTOI

-- | Used for pruning immediately identical boards.
identicalStates :: Piece -> Rotation -> (Position, Int)
-- ^ Returns an offset and a identification number that is the same
-- for identical positions.
-- The offset is to be subtracted from the current position.
identicalStates PieceEmpty rot = ((0,0), 0)
identicalStates PieceZ 0 = ((0,0),1)
identicalStates PieceZ 1 = ((0,0),2)
identicalStates PieceZ 2 = ((0,1),1)
identicalStates PieceZ 3 = ((1,0),2)
identicalStates PieceS 0 = ((0,0),3)
identicalStates PieceS 1 = ((0,0),4)
identicalStates PieceS 2 = ((0,1),3)
identicalStates PieceS 3 = ((1,0),4)
identicalStates PieceJ rot = ((0,0),rot+5)
identicalStates PieceL rot = ((0,0),rot+9)
identicalStates PieceT rot = ((0,0),rot+13)
identicalStates PieceO rot = ((0,0),17)
identicalStates PieceI 0 = ((0,0),18)
identicalStates PieceI 1 = ((0,0),19)
identicalStates PieceI 2 = ((0,1),18)
identicalStates PieceI 3 = ((1,0),19)
identicalStates _ _ = error "Rotation out of bounds."

canonicalState :: Int -> Rotation
canonicalState n | n < 20 = [0,0,1,0,1, 0,1,2,3,0, 1,2,3,0,1, 2,3,0,0,1] !! n
canonicalState _ = error "Canonical state id out of bounds"
