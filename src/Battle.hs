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
import Control.Monad.ST
import Data.STRef
import Control.Monad

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
naiveStack fb = -holePenalty-stackPenalty-meanHeight
    where
        ((0,0), (x0,y0)) = bounds fb
        stacks = map
            (\x -> dropWhile not (map (\y -> fb!(x,y)) [y0,y0-1..0]))
            [0..x0]
        holePenalty = int2Float $ sum (map (length . filter not) stacks)+1
        heights = map (int2Float . length) stacks
        meanHeight = sum heights / int2Float (x0+1)
        heightDifferences [c] = []
        heightDifferences (c1:c2:cs) = (c1-c2):heightDifferences (c2:cs)
        heightDifferences [] = []
        stackPenalty = sqrt(
            sum (map ((**2) . (meanHeight-)) heights)
                / int2Float (x0+1) )
                    + sum (map (**2) $ heightDifferences heights) / int2Float (x0+1) +1

searchMove :: forall ia ka sa s. (IArray ia Bool, IArray ka Position, IArray sa Position)
           => (Position, Position) -- ^ Safe region
           -> KickTable ka
           -> [Move]  -- ^ Available moves
           -> sa Piece Position -- ^ Spawn position
           -> [Piece]
           -> (FrozenField ia -> Float)  -- ^ Evaluation
           -> Int -- ^ Iteration count
           -> Float -- ^ Exploration factor
           -> FrozenField ia
           -> ST s (SearchTree s (FrozenField ia, [Piece]) [Move], [Move])
           -- TODO reuse the tree
searchMove sr kt mvs sp pcs ev it ex fb = do
    est <- newSTRef (ev fb, 1)
    tree <- newSTRef (Leaf (fb,pcs) est)
    replicateM_ it (mctsIterate ex branch (const True) (ev.fst) tree)
    searchedTree <- readSTRef tree
    Just m <- chooseMove searchedTree
    subTree <- makeMove m searchedTree
    return (subTree, m)
    where
        branch :: (FrozenField ia, [Piece]) -> [([Move], (FrozenField ia, [Piece]))]
        branch (fb, []) = []
        branch (fb, pc:pcs) = mapMaybe calculate $
            Map.assocs (allPlacements fb sr kt mvs pc (sp ! pc,0))
            where
                calculate :: (((Position, Rotation), Bool), [Move])
                          -> Maybe ([Move], (FrozenField ia, [Piece]))
                calculate ((_,False),_) = Nothing
                calculate ((st,True),mvs) = Just (mvs, (fb', pcs))
                    where
                        fb' = fst $ clearLine $ lock fb pc st
