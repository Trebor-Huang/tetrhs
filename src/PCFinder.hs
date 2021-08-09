{-# LANGUAGE FlexibleContexts #-}
module PCFinder where

import Board
import SearchAlgorithms
import Data.Array.IArray
import Data.Maybe
import Control.Lens
import Finesse
import qualified Data.Map as Map
import GHC.IO

-- The total number of blocks should be a multiple of the board width
blockCount :: (IArray ia Bool) => FrozenField ia -> Int
blockCount fb = length $ filter id $ elems fb

blockCountCheck :: (IArray ia Bool)
                => FrozenField ia
                -> [Piece]
                -> (Int, Bool)
blockCountCheck fb pcs = (b`div`y, b`mod`y==0)
    where
        b = blockCount fb + sum (map (blockCount . flip pieceShape 0) pcs)
        y = (snd $ snd $ bounds fb) + 1

-- Search!
searchPC :: (IArray fa Bool, IArray ka Position, IArray sa Position)
         => FrozenField fa
         -> (Position, Position) -- ^ Safe region
         -> KickTable ka
         -> [Move] -- ^ available moves
         -> sa Piece Position -- ^ spawn location  -- TODO add spawn rotation?
         -> [Piece]  -- TODO holding
         -> [[[Move]]]  -- TODO heuristics
-- | Returns a list of PC solutions, each of which is a list,
-- whose each item corresponds to a piece, and gives a list of moves that places the piece
searchPC fb sr kt mv sl pcs | chk = map (reverse . (^._1)) (bfs
    (isPC . (^._2))  -- Search for PC'd boards
    noOverspill
    ([], fb, pcs, lno)  -- Starting from (No move, Initial Board, Pieces)
    (\(mvs, fb, pcs, lno) ->
        case pcs of
            (pc:pcs') -> [ ((movemap Map.! (st, True)):mvs, fb', pcs', lno-lno') |
                let movemap = allPlacements fb sr kt mv pc (sl!pc,0),
                (st, True) <- Map.keys movemap,
                let (fb', lno') = clearLine $ lock fb pc st]
            [] -> []))
                            | otherwise = []
    where
        (lno, chk) = blockCountCheck fb pcs
        noOverspill :: IArray fa Bool => (a, FrozenField fa, b, Int) -> Bool
        noOverspill (_, fb, _, lno') =
            and [
                not $ or [ fb ! (x, y) | x <- [0..fst(snd(bounds fb))]]
                | y <- [lno'..snd(snd(bounds fb))]]
