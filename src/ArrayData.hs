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

guideLineFinesseTable :: [(Piece, (Position, Rotation), [Move])]
guideLineFinesseTable
    = [(PieceZ,((-1,0),1),[MRRight,MDASLeft,MSoft]),(PieceZ,((0,-1),0),[MDASLeft,MSoft]),(PieceZ,((0,0),1),[MDASLeft,MRRight,MSoft]),(PieceZ,((0,0),2),[MDASLeft,MRLeft,MRLeft,MSoft]),(PieceZ,((0,0),3),[MDASLeft,MRLeft,MSoft]),(PieceZ,((1,-1),0),[MDASLeft,MRight,MSoft]),(PieceZ,((1,0),1),[MDASLeft,MRRight,MRight,MSoft]),(PieceZ,((1,0),2),[MDASLeft,MRLeft,MRLeft,MRight,MSoft]),(PieceZ,((1,0),3),[MDASLeft,MRLeft,MRight,MSoft]),(PieceZ,((2,-1),0),[MLeft,MSoft]),(PieceZ,((2,0),1),[MRRight,MLeft,MSoft]),(PieceZ,((2,0),2),[MRLeft,MRLeft,MLeft,MSoft]),(PieceZ,((2,0),3),[MRLeft,MLeft,MSoft]),(PieceZ,((3,-1),0),[MSoft]),(PieceZ,((3,0),1),[MRRight,MSoft]),(PieceZ,((3,0),2),[MRLeft,MRLeft,MSoft]),(PieceZ,((3,0),3),[MRLeft,MSoft]),(PieceZ,((4,-1),0),[MRight,MSoft]),(PieceZ,((4,0),1),[MRRight,MRight,MSoft]),(PieceZ,((4,0),2),[MRLeft,MRLeft,MRight,MSoft]),(PieceZ,((4,0),3),[MRLeft,MRight,MSoft]),(PieceZ,((5,-1),0),[MRight,MRight,MSoft]),(PieceZ,((5,0),1),[MRRight,MRight,MRight,MSoft]),(PieceZ,((5,0),2),[MRLeft,MRLeft,MRight,MRight,MSoft]),(PieceZ,((5,0),3),[MRLeft,MRight,MRight,MSoft]),(PieceZ,((6,-1),0),[MDASRight,MLeft,MSoft]),(PieceZ,((6,0),1),[MDASRight,MRRight,MLeft,MSoft]),(PieceZ,((6,0),2),[MDASRight,MRLeft,MRLeft,MLeft,MSoft]),(PieceZ,((6,0),3),[MDASRight,MRLeft,MLeft,MSoft]),(PieceZ,((7,-1),0),[MDASRight,MSoft]),(PieceZ,((7,0),1),[MDASRight,MRRight,MSoft]),(PieceZ,((7,0),2),[MDASRight,MRLeft,MRLeft,MSoft]),(PieceZ,((7,0),3),[MDASRight,MRLeft,MSoft]),(PieceZ,((8,0),3),[MRLeft,MDASRight,MSoft]),(PieceS,((-1,0),1),[MRRight,MDASLeft,MSoft]),(PieceS,((0,-1),0),[MDASLeft,MSoft]),(PieceS,((0,0),1),[MDASLeft,MRRight,MSoft]),(PieceS,((0,0),2),[MDASLeft,MRLeft,MRLeft,MSoft]),(PieceS,((0,0),3),[MDASLeft,MRLeft,MSoft]),(PieceS,((1,-1),0),[MDASLeft,MRight,MSoft]),(PieceS,((1,0),1),[MDASLeft,MRRight,MRight,MSoft]),(PieceS,((1,0),2),[MDASLeft,MRLeft,MRLeft,MRight,MSoft]),(PieceS,((1,0),3),[MDASLeft,MRLeft,MRight,MSoft]),(PieceS,((2,-1),0),[MLeft,MSoft]),(PieceS,((2,0),1),[MRRight,MLeft,MSoft]),(PieceS,((2,0),2),[MRLeft,MRLeft,MLeft,MSoft]),(PieceS,((2,0),3),[MRLeft,MLeft,MSoft]),(PieceS,((3,-1),0),[MSoft]),(PieceS,((3,0),1),[MRRight,MSoft]),(PieceS,((3,0),2),[MRLeft,MRLeft,MSoft]),(PieceS,((3,0),3),[MRLeft,MSoft]),(PieceS,((4,-1),0),[MRight,MSoft]),(PieceS,((4,0),1),[MRRight,MRight,MSoft]),(PieceS,((4,0),2),[MRLeft,MRLeft,MRight,MSoft]),(PieceS,((4,0),3),[MRLeft,MRight,MSoft]),(PieceS,((5,-1),0),[MRight,MRight,MSoft]),(PieceS,((5,0),1),[MRRight,MRight,MRight,MSoft]),(PieceS,((5,0),2),[MRLeft,MRLeft,MRight,MRight,MSoft]),(PieceS,((5,0),3),[MRLeft,MRight,MRight,MSoft]),(PieceS,((6,-1),0),[MDASRight,MLeft,MSoft]),(PieceS,((6,0),1),[MDASRight,MRRight,MLeft,MSoft]),(PieceS,((6,0),2),[MDASRight,MRLeft,MRLeft,MLeft,MSoft]),(PieceS,((6,0),3),[MDASRight,MRLeft,MLeft,MSoft]),(PieceS,((7,-1),0),[MDASRight,MSoft]),(PieceS,((7,0),1),[MDASRight,MRRight,MSoft]),(PieceS,((7,0),2),[MDASRight,MRLeft,MRLeft,MSoft]),(PieceS,((7,0),3),[MDASRight,MRLeft,MSoft]),(PieceS,((8,0),3),[MRLeft,MDASRight,MSoft]),(PieceJ,((-1,0),1),[MRRight,MDASLeft,MSoft]),(PieceJ,((0,-1),0),[MDASLeft,MSoft]),(PieceJ,((0,0),1),[MDASLeft,MRRight,MSoft]),(PieceJ,((0,0),2),[MDASLeft,MRLeft,MRLeft,MSoft]),(PieceJ,((0,0),3),[MDASLeft,MRLeft,MSoft]),(PieceJ,((1,-1),0),[MDASLeft,MRight,MSoft]),(PieceJ,((1,0),1),[MDASLeft,MRRight,MRight,MSoft]),(PieceJ,((1,0),2),[MDASLeft,MRLeft,MRLeft,MRight,MSoft]),(PieceJ,((1,0),3),[MDASLeft,MRLeft,MRight,MSoft]),(PieceJ,((2,-1),0),[MLeft,MSoft]),(PieceJ,((2,0),1),[MRRight,MLeft,MSoft]),(PieceJ,((2,0),2),[MRLeft,MRLeft,MLeft,MSoft]),(PieceJ,((2,0),3),[MRLeft,MLeft,MSoft]),(PieceJ,((3,-1),0),[MSoft]),(PieceJ,((3,0),1),[MRRight,MSoft]),(PieceJ,((3,0),2),[MRLeft,MRLeft,MSoft]),(PieceJ,((3,0),3),[MRLeft,MSoft]),(PieceJ,((4,-1),0),[MRight,MSoft]),(PieceJ,((4,0),1),[MRRight,MRight,MSoft]),(PieceJ,((4,0),2),[MRLeft,MRLeft,MRight,MSoft]),(PieceJ,((4,0),3),[MRLeft,MRight,MSoft]),(PieceJ,((5,-1),0),[MRight,MRight,MSoft]),(PieceJ,((5,0),1),[MRRight,MRight,MRight,MSoft]),(PieceJ,((5,0),2),[MRLeft,MRLeft,MRight,MRight,MSoft]),(PieceJ,((5,0),3),[MRLeft,MRight,MRight,MSoft]),(PieceJ,((6,-1),0),[MDASRight,MLeft,MSoft]),(PieceJ,((6,0),1),[MDASRight,MRRight,MLeft,MSoft]),(PieceJ,((6,0),2),[MDASRight,MRLeft,MRLeft,MLeft,MSoft]),(PieceJ,((6,0),3),[MDASRight,MRLeft,MLeft,MSoft]),(PieceJ,((7,-1),0),[MDASRight,MSoft]),(PieceJ,((7,0),1),[MDASRight,MRRight,MSoft]),(PieceJ,((7,0),2),[MDASRight,MRLeft,MRLeft,MSoft]),(PieceJ,((7,0),3),[MDASRight,MRLeft,MSoft]),(PieceJ,((8,0),3),[MRLeft,MDASRight,MSoft]),(PieceL,((-1,0),1),[MRRight,MDASLeft,MSoft]),(PieceL,((0,-1),0),[MDASLeft,MSoft]),(PieceL,((0,0),1),[MDASLeft,MRRight,MSoft]),(PieceL,((0,0),2),[MDASLeft,MRLeft,MRLeft,MSoft]),(PieceL,((0,0),3),[MDASLeft,MRLeft,MSoft]),(PieceL,((1,-1),0),[MDASLeft,MRight,MSoft]),(PieceL,((1,0),1),[MDASLeft,MRRight,MRight,MSoft]),(PieceL,((1,0),2),[MDASLeft,MRLeft,MRLeft,MRight,MSoft]),(PieceL,((1,0),3),[MDASLeft,MRLeft,MRight,MSoft]),(PieceL,((2,-1),0),[MLeft,MSoft]),(PieceL,((2,0),1),[MRRight,MLeft,MSoft]),(PieceL,((2,0),2),[MRLeft,MRLeft,MLeft,MSoft]),(PieceL,((2,0),3),[MRLeft,MLeft,MSoft]),(PieceL,((3,-1),0),[MSoft]),(PieceL,((3,0),1),[MRRight,MSoft]),(PieceL,((3,0),2),[MRLeft,MRLeft,MSoft]),(PieceL,((3,0),3),[MRLeft,MSoft]),(PieceL,((4,-1),0),[MRight,MSoft]),(PieceL,((4,0),1),[MRRight,MRight,MSoft]),(PieceL,((4,0),2),[MRLeft,MRLeft,MRight,MSoft]),(PieceL,((4,0),3),[MRLeft,MRight,MSoft]),(PieceL,((5,-1),0),[MRight,MRight,MSoft]),(PieceL,((5,0),1),[MRRight,MRight,MRight,MSoft]),(PieceL,((5,0),2),[MRLeft,MRLeft,MRight,MRight,MSoft]),(PieceL,((5,0),3),[MRLeft,MRight,MRight,MSoft]),(PieceL,((6,-1),0),[MDASRight,MLeft,MSoft]),(PieceL,((6,0),1),[MDASRight,MRRight,MLeft,MSoft]),(PieceL,((6,0),2),[MDASRight,MRLeft,MRLeft,MLeft,MSoft]),(PieceL,((6,0),3),[MDASRight,MRLeft,MLeft,MSoft]),(PieceL,((7,-1),0),[MDASRight,MSoft]),(PieceL,((7,0),1),[MDASRight,MRRight,MSoft]),(PieceL,((7,0),2),[MDASRight,MRLeft,MRLeft,MSoft]),(PieceL,((7,0),3),[MDASRight,MRLeft,MSoft]),(PieceL,((8,0),3),[MRLeft,MDASRight,MSoft]),(PieceT,((-1,0),1),[MRRight,MDASLeft,MSoft]),(PieceT,((0,-1),0),[MDASLeft,MSoft]),(PieceT,((0,0),1),[MDASLeft,MRRight,MSoft]),(PieceT,((0,0),2),[MDASLeft,MRLeft,MRLeft,MSoft]),(PieceT,((0,0),3),[MDASLeft,MRLeft,MSoft]),(PieceT,((1,-1),0),[MDASLeft,MRight,MSoft]),(PieceT,((1,0),1),[MDASLeft,MRRight,MRight,MSoft]),(PieceT,((1,0),2),[MDASLeft,MRLeft,MRLeft,MRight,MSoft]),(PieceT,((1,0),3),[MDASLeft,MRLeft,MRight,MSoft]),(PieceT,((2,-1),0),[MLeft,MSoft]),(PieceT,((2,0),1),[MRRight,MLeft,MSoft]),(PieceT,((2,0),2),[MRLeft,MRLeft,MLeft,MSoft]),(PieceT,((2,0),3),[MRLeft,MLeft,MSoft]),(PieceT,((3,-1),0),[MSoft]),(PieceT,((3,0),1),[MRRight,MSoft]),(PieceT,((3,0),2),[MRLeft,MRLeft,MSoft]),(PieceT,((3,0),3),[MRLeft,MSoft]),(PieceT,((4,-1),0),[MRight,MSoft]),(PieceT,((4,0),1),[MRRight,MRight,MSoft]),(PieceT,((4,0),2),[MRLeft,MRLeft,MRight,MSoft]),(PieceT,((4,0),3),[MRLeft,MRight,MSoft]),(PieceT,((5,-1),0),[MRight,MRight,MSoft]),(PieceT,((5,0),1),[MRRight,MRight,MRight,MSoft]),(PieceT,((5,0),2),[MRLeft,MRLeft,MRight,MRight,MSoft]),(PieceT,((5,0),3),[MRLeft,MRight,MRight,MSoft]),(PieceT,((6,-1),0),[MDASRight,MLeft,MSoft]),(PieceT,((6,0),1),[MDASRight,MRRight,MLeft,MSoft]),(PieceT,((6,0),2),[MDASRight,MRLeft,MRLeft,MLeft,MSoft]),(PieceT,((6,0),3),[MDASRight,MRLeft,MLeft,MSoft]),(PieceT,((7,-1),0),[MDASRight,MSoft]),(PieceT,((7,0),1),[MDASRight,MRRight,MSoft]),(PieceT,((7,0),2),[MDASRight,MRLeft,MRLeft,MSoft]),(PieceT,((7,0),3),[MDASRight,MRLeft,MSoft]),(PieceT,((8,0),3),[MRLeft,MDASRight,MSoft]),(PieceO,((0,0),0),[MDASLeft,MSoft]),(PieceO,((0,0),1),[MDASLeft,MRRight,MSoft]),(PieceO,((0,0),2),[MDASLeft,MRLeft,MRLeft,MSoft]),(PieceO,((0,0),3),[MDASLeft,MRLeft,MSoft]),(PieceO,((1,0),0),[MDASLeft,MRight,MSoft]),(PieceO,((1,0),1),[MDASLeft,MRRight,MRight,MSoft]),(PieceO,((1,0),2),[MDASLeft,MRLeft,MRLeft,MRight,MSoft]),(PieceO,((1,0),3),[MDASLeft,MRLeft,MRight,MSoft]),(PieceO,((2,0),0),[MLeft,MLeft,MSoft]),(PieceO,((2,0),1),[MRRight,MLeft,MLeft,MSoft]),(PieceO,((2,0),2),[MRLeft,MRLeft,MLeft,MLeft,MSoft]),(PieceO,((2,0),3),[MRLeft,MLeft,MLeft,MSoft]),(PieceO,((3,0),0),[MLeft,MSoft]),(PieceO,((3,0),1),[MRRight,MLeft,MSoft]),(PieceO,((3,0),2),[MRLeft,MRLeft,MLeft,MSoft]),(PieceO,((3,0),3),[MRLeft,MLeft,MSoft]),(PieceO,((4,0),0),[MSoft]),(PieceO,((4,0),1),[MRRight,MSoft]),(PieceO,((4,0),2),[MRLeft,MRLeft,MSoft]),(PieceO,((4,0),3),[MRLeft,MSoft]),(PieceO,((5,0),0),[MRight,MSoft]),(PieceO,((5,0),1),[MRRight,MRight,MSoft]),(PieceO,((5,0),2),[MRLeft,MRLeft,MRight,MSoft]),(PieceO,((5,0),3),[MRLeft,MRight,MSoft]),(PieceO,((6,0),0),[MRight,MRight,MSoft]),(PieceO,((6,0),1),[MRRight,MRight,MRight,MSoft]),(PieceO,((6,0),2),[MRLeft,MRLeft,MRight,MRight,MSoft]),(PieceO,((6,0),3),[MRLeft,MRight,MRight,MSoft]),(PieceO,((7,0),0),[MDASRight,MLeft,MSoft]),(PieceO,((7,0),1),[MDASRight,MRRight,MLeft,MSoft]),(PieceO,((7,0),2),[MDASRight,MRLeft,MRLeft,MLeft,MSoft]),(PieceO,((7,0),3),[MDASRight,MRLeft,MLeft,MSoft]),(PieceO,((8,0),0),[MDASRight,MSoft]),(PieceO,((8,0),1),[MDASRight,MRRight,MSoft]),(PieceO,((8,0),2),[MDASRight,MRLeft,MRLeft,MSoft]),(PieceO,((8,0),3),[MDASRight,MRLeft,MSoft]),(PieceI,((-2,0),1),[MRRight,MDASLeft,MSoft]),(PieceI,((-1,0),1),[MDASLeft,MRRight,MLeft,MSoft]),(PieceI,((-1,0),3),[MRLeft,MDASLeft,MSoft]),(PieceI,((0,-2),0),[MDASLeft,MSoft]),(PieceI,((0,-1),2),[MDASLeft,MRLeft,MRLeft,MSoft]),(PieceI,((0,0),1),[MDASLeft,MRRight,MSoft]),(PieceI,((0,0),3),[MDASLeft,MRLeft,MSoft]),(PieceI,((1,-2),0),[MDASLeft,MRight,MSoft]),(PieceI,((1,-1),2),[MDASLeft,MRLeft,MRLeft,MRight,MSoft]),(PieceI,((1,0),1),[MDASLeft,MRRight,MRight,MSoft]),(PieceI,((1,0),3),[MDASLeft,MRLeft,MRight,MSoft]),(PieceI,((2,-2),0),[MLeft,MSoft]),(PieceI,((2,-1),2),[MRLeft,MRLeft,MLeft,MSoft]),(PieceI,((2,0),1),[MRRight,MLeft,MSoft]),(PieceI,((2,0),3),[MRLeft,MLeft,MSoft]),(PieceI,((3,-2),0),[MSoft]),(PieceI,((3,-1),2),[MRLeft,MRLeft,MSoft]),(PieceI,((3,0),1),[MRRight,MSoft]),(PieceI,((3,0),3),[MRLeft,MSoft]),(PieceI,((4,-2),0),[MRight,MSoft]),(PieceI,((4,-1),2),[MRLeft,MRLeft,MRight,MSoft]),(PieceI,((4,0),1),[MRRight,MRight,MSoft]),(PieceI,((4,0),3),[MRLeft,MRight,MSoft]),(PieceI,((5,-2),0),[MDASRight,MLeft,MSoft]),(PieceI,((5,-1),2),[MDASRight,MRLeft,MRLeft,MLeft,MSoft]),(PieceI,((5,0),1),[MDASRight,MRRight,MLeft,MSoft]),(PieceI,((5,0),3),[MDASRight,MRLeft,MLeft,MSoft]),(PieceI,((6,-2),0),[MDASRight,MSoft]),(PieceI,((6,-1),2),[MDASRight,MRLeft,MRLeft,MSoft]),(PieceI,((6,0),1),[MDASRight,MRRight,MSoft]),(PieceI,((6,0),3),[MDASRight,MRLeft,MSoft]),(PieceI,((7,0),1),[MRRight,MDASRight,MSoft]),(PieceI,((7,0),3),[MDASRight,MRLeft,MRight,MSoft]),(PieceI,((8,0),3),[MRLeft,MDASRight,MSoft])]

-- TODO: actual guideline finesse table
