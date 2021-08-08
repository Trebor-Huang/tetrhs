{-# LANGUAGE FlexibleContexts #-}
module PCFinder where

import Board
import SearchAlgorithms
import Data.Array.IArray
import Data.Maybe
import Control.Lens
import Finesse
import qualified Data.Map as Map

-- The total number of blocks should be a multiple of the board width
blockCount :: (IArray ia Bool) => FrozenField ia -> Int
blockCount fb = length $ filter id $ elems fb

blockCountCheck :: (IArray ia Bool)
                => FrozenField ia
                -> [Piece]
                -> Bool
blockCountCheck fb pcs =
    even (blockCount fb + sum (map (blockCount . flip pieceShape 0) pcs))

-- Search!

searchPC :: (IArray fa Bool, IArray ka Position, IArray sa Position)
         => FrozenField fa
         -> (Position, Position) -- Safe region
         -> KickTable ka
         -> [Move] -- available moves
         -> sa Piece Position -- spawn location  -- TODO add spawn rotation?
         -> [Piece]
         -> Maybe [[Move]]
searchPC fb sr kt mv sl pcs = reverse . (^._1) <$> listToMaybe (bfs
    (isPC . (^._2))  -- Search for PC'd boards
    ([], fb, pcs)  -- Starting from (No move, Initial Board, Pieces)
    (\(mvs, fb, pcs) ->
        case pcs of
            (pc:pcs') -> [ ((movemap Map.! (st, True)):mvs, clearLine $ lock fb pc st, pcs') |
                let movemap = allPlacements fb sr kt mv pc (sl!pc,0),
                (st, True) <- Map.keys movemap]
            [] -> [])) -- ? Should we return singleton instead?
