{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Battle where

import Data.Array.IArray
import Board
import Finesse
import SearchAlgorithms
import Control.Lens
import GHC.Float
import qualified Data.Map as Map
import Data.Maybe

-- | Given board and piece state, returns whether spinning results in
-- a T-Spin, by the tri-corner criterion.
isTSpin :: (IArray ia Bool) => ia Position Bool -> (Position, Rotation) -> Bool
isTSpin fb st = False -- TODO

-- | Number of garbage sent under different circumstances, used for evaluation.
data GarbageData ia = GarbageData
    {
        _pc :: Int,
        _basic :: ia (Int, Int, Bool) Int -- [Clear Line, Ren, is spin] -> Garbage line
    }

-- | All possible sequences, accounting for holds.
possibleSequences :: Bool  -- ^ Currently is hold available?
                  -> [Piece]  -- ^ Incoming pieces
                  -> Piece  -- ^ Piece on hold
                  -> [([(Piece, Bool)], Piece)]
-- ^ Returns possible sequences, together with a boolean indicating whether
-- to hold before placing this piece.
-- TODO make this more efficient
possibleSequences False [] pc = [([], pc)]  -- Can't deduce what comes in with hold
possibleSequences True [] PieceEmpty = [([], PieceEmpty)]
possibleSequences True [] pc = [([], pc), ([(pc, True)], PieceEmpty)]
possibleSequences False (pc':pcs) pc = map (_1 %~ ((pc',False):)) (possibleSequences True pcs pc)
possibleSequences True (pc':pcs) pc | pc /= PieceEmpty =
    map (_1 %~ ((pc,True):)) (possibleSequences False pcs pc')
        ++ possibleSequences False (pc':pcs) pc
                                    | otherwise =
    map pressHold (possibleSequences False pcs pc') ++ possibleSequences False (pc':pcs) pc
            where pressHold (seq, holding) = case seq of
                    [] -> ([], holding)
                    (p,False):ps -> ((p, True) : ps, holding)
                    _ -> error "Impossible!"

naiveStack :: (IArray ia Bool) => FrozenField ia -> Float
naiveStack fb = exp (holePenalty + stackPenalty)
    where
        ((0,0), (x0,y0)) = bounds fb
        stacks = map
            (\x -> dropWhile not (map (\y -> fb!(x,y)) [x0,x0-1..0]))
            [0..y0]
        holePenalty = - int2Float (sum (map (length . filter not) stacks))
        heights = map (int2Float . length) stacks
        minHeight = foldr min (int2Float y0) heights
        meanHeight = (sum heights - minHeight) / int2Float x0
        minHeightContribution = (meanHeight - minHeight)**2
        stackPenalty = - sqrt(
            (sum (map ((**2) . (meanHeight-)) heights) - minHeightContribution)
                / int2Float x0 )

searchNext :: forall ia ka sa. (IArray ia Bool, IArray ka Position, IArray sa Position)
           => (Position, Position)  -- ^ Safe region
           -> KickTable ka
           -> sa Piece Position
           -> [Move] -- ^ Available moves
           -> FrozenField ia
           -> [Piece]
           -> Int
           -> [Move]
searchNext sr kt sp mvs fb pcs = heuristicSearch transitions (naiveStack.fst) (const True) (fb,pcs)
    where
        transitions :: (FrozenField ia, [Piece])
                    -> [([Move], (FrozenField ia, [Piece]))]
        transitions (fb, []) = []
        transitions (fb, pc:pcs) = mapMaybe (getNext pc pcs)
                (Map.assocs $ allPlacements fb sr kt mvs pc (sp!pc, 0))

        getNext :: Piece -> [Piece]
                -> (((Position, Rotation), Bool), [Move])
                -> Maybe ([Move], (FrozenField ia, [Piece]))
        getNext pc pcs ((st, False), mvs) = Nothing
        getNext pc pcs ((st, True), mvs) = Just (mvs, (fst $ clearLine $ lock fb pc st, pcs))
