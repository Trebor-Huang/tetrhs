{-# LANGUAGE FlexibleContexts #-}
module Finesse where

import Board
import SearchAlgorithms
import Data.Array.IArray
import qualified Data.Map as Map
import Control.Lens
import Data.Maybe

searchFinesse :: (IArray a Bool, IArray ia Position)
            => a Position Bool  -- frozen board
            -> (Int, Int) -- Size
            -> (Position, Position) -- safe region
            -> KickTable ia
            -> [Move] -- available moves
            -> Piece
            -> (Position, Rotation) -- start state
            -> ((Position, Rotation), Bool) -- end state, lock?
            -> Maybe [Move]
searchFinesse fb sz sr kt mv pc st
    = bfsRoute (((sr^._1, 0), False), ((sr^._2, 3), True))
        (\(s, locked) -> [ (m, (s', locked')) |
            not locked,
            m <- mv,
            let (s', b) = computeMove fb sz kt pc s m,
            locked' <- if m == MSoft then [True, False] else [False],
            b || locked']) (st, False)

standardMoves = [MRLeft, MRRight, MDASLeft, MDASRight, MLeft, MRight, MSoft, MDown]

allPlacements :: (IArray a Bool, IArray ia Position)
            => a Position Bool  -- frozen board
            -> (Int, Int) -- Size
            -> (Position, Position) -- safe region
            -> KickTable ia
            -> [Move] -- available moves
            -> Piece
            -> (Position, Rotation) -- start state
            -> Map.Map ((Position, Rotation), Bool) [Move]
allPlacements fb sz sr kt mv pc st =
    let srch = bfsRouteArray
            (((sr^._1, 0), False), ((sr^._2, 3), True))
            (\(s, locked)
                -> [(m, (s', locked')) |
                    not locked,
                    m <- mv,
                    let (s', b) = computeMove fb sz kt pc s m,
                    locked' <- if m == MSoft then [True, False] else [False],
                    b || locked'])
            (st, False) in
    Map.fromDistinctAscList
    [((st', lk), fromJust mmvs) |
        st' <- range ((sr^._1, 0), (sr^._2, 3)),
        lk <- [False, True],
        let mmvs = srch ! (st', lk),
        isJust mmvs]
