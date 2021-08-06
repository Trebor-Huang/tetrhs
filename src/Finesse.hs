{-# LANGUAGE FlexibleContexts #-}
module Finesse where

import Board
import SearchAlgorithms

searchFinesse :: BoardConstraint ma ia m
              => [Move]  -- available moves
              -> Board ma ia m
              -> Position
              -> Rotation
              -> [Move]
{-
    This is nothing but a dynamic programming problem.
    We will dp over the space (Position, Rotation).
-}
searchFinesse goodMove board endPos endRot = undefined
