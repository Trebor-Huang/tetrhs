{-# LANGUAGE FlexibleContexts #-}
module Finesse where

import Board
import SearchAlgorithms
import Data.Array.IArray
import Control.Lens

searchFinesse :: (IArray a Bool, IArray ia Position)
            => a Position Bool  -- frozen board
            -> (Int, Int) -- Size
            -> (Position, Position) -- safe region
            -> KickTable ia
            -> [Move] -- available moves
            -> Piece
            -> (Position, Rotation) -- start state
            -> (Position, Rotation) -- end state
            -> Maybe [Move]
searchFinesse fb sz sr kt mv pc
    = bfsRoute ((sr^._1, 0), (sr^._2, 3))
        (\s -> [ (m, s') |
            m <- mv,
            let (s', b) = computeMove fb sz kt pc s m,
            b])

standardMoves = [MDASLeft, MDASRight, MRLeft, MRRight, MLeft, MRight, MSoft, MDown]
