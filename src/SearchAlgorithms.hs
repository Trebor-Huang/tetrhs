{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
module SearchAlgorithms where
-- Standalone module for searching

import Control.Monad.Loops
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Random
import Data.Array
import Data.Array.IO
import Data.Array.ST
import Data.Maybe
import Data.STRef
import Data.Function
import Control.Lens
import qualified Data.Map as Map
import Data.List
import GHC.Float
import GHC.Generics

bfs :: (a -> Bool) -- ^ Goal if evaluates to True
    -> (a -> Bool) -- ^ Prune if evaluates to False
    -> a
    -> (a -> [a])
    -> [a]
bfs predicate prune a0 branch = filter predicate searchspace
    where
        -- An elegant solution
        -- searchspace = a0 : (branch =<< searchspace)
        -- However, this solution <<loop>>'s when the search is finite
        searchspace = concat $ takeWhile (not.null) epochs
        epochs = [a0] : map (\as -> [ a' | a <- as, prune a, a' <- branch a]) epochs

memoST :: forall a b. (Ix a)
     => (a, a)    -- range of the argument memoized
     -> ((a -> b) -- a recursive function, but uses it's first argument for recursive calls instead
       -> a -> b)
     -> (a -> b)  -- memoized function
memoST r f = unpack
    where
        unpack n =  arr ! n

        arr = runSTArray compute

        compute :: ST s (STArray s a b)
        compute = do
            a <- newArray_ r
            forM_ (range r) $ \i -> do
                writeArray a i $ f unpack i
            return a

-- TODO profile bidirectional search
bfsRouteArray :: forall a c. (Ix a)
    => (a, a)  -- on a range of vertices of type a
    -> (a -> [(c, a)])  -- shows a list of connected vertices, c is the edge type
    -> a -> Array a (Maybe [c]) -- finds a shortest route between two points
bfsRouteArray (!r) f (!a) = fetch
    where
        unpack n = arr ! n
        arr = runSTArray compute
        compute :: ST s (STArray s a (Maybe (a, c)))
        compute = do
            array <- newArray r Nothing :: ST s (STArray s a (Maybe (a, c)))  -- get an empty array
            neighref <- newSTRef [a]  -- a starting point
            whileM_
                (not . null <$> readSTRef neighref)  -- while there are still neighbours to visit
                (do
                    neighbours <- readSTRef neighref
                    new_neighbours <- forM
                        neighbours  -- for each neighbour
                        (\a0 -> do
                            let nodes = f a0  -- get the connected nodes
                            forM
                                nodes
                                (\(c, n) -> do
                                    r <- readArray array n
                                    case r of
                                        Nothing -> do
                                            writeArray array n (Just (a0, c))
                                            return (Just n)
                                        Just c' -> return Nothing))
                    writeSTRef neighref (catMaybes $ join new_neighbours))
            -- now we get a array of (previous node, last-step-taken)
            -- we just need to follow it to build up a complete route
            return array
        trace route a1 | a1 == a = route
                       | otherwise = do
                            (a',c) <- unpack a1
                            rt <- route
                            trace (Just (c:rt)) a'
        fetch = listArray r $ map (trace (Just [])) (range r)

bfsRoute r f a = (fetch!)
    where
        fetch = bfsRouteArray r f a

-- TODO ST MONAD should be much faster
data SearchTree a c =
    Leaf {_node :: a, _totalVisit :: Float, _totalEstimation :: Float} |
    Branch {_node :: a, _children :: Map.Map c (SearchTree a c), _totalVisit :: Float, _totalEstimation :: Float}
    deriving (Generic)
makeLenses ''SearchTree

instance (Show a, Show c, Ord c) => Show (SearchTree a c) where
    show (Leaf n v e) = "[" ++ show n ++ "] (" ++ show e ++ "/" ++ show v ++ ")"
    show (Branch n m v e) = "[" ++ show n ++ "] (" ++ show e ++ "/" ++ show v ++ ")"
        ++ concat [ "\n+ " ++ show k ++ "\n|- " ++ intercalate "\n| " (lines (show (m Map.! k)))  | k <- Map.keys m]

exploration :: Float
exploration = 1.4142

heuristicSearchIter :: (Ord c)
    => (a -> [(c, a)])  -- ^ A list of connected vertices, `c` is the edge type.
    -> (a -> Float)  -- ^ An evaluation function.
    -> (a -> Bool)  -- ^ A pruning function. The branch is pruned when this returns False
    -> SearchTree a c -> SearchTree a c
heuristicSearchIter branches evaluate prune searchTree =
    let (node, update) = selectNode searchTree in
    let result = [ (c, a, evaluate a) | (c, a) <- branches node, prune a] in
        update (Map.fromList [ (c, Leaf a 1 e) | (c, a, e) <- result],
            sum (map (^._3) result),
            int2Float $ length result)
    where
        -- could be better with zippers, but whatever, I'm going to use ST later anyway
        selectNode :: (Ord c) => SearchTree a c -> (a, (Map.Map c (SearchTree a c), Float,Float) -> SearchTree a c)
        selectNode (Leaf n v e) = (n, \(c',e',v') -> Branch n c' (v+v') (e+e'))
        selectNode (Branch n c v e) =
            let child = maximumBy (compare `on`
                    (\k -> confidence v (c Map.! k ^.totalVisit) (c Map.! k ^.totalEstimation))) (Map.keys c) in
            let (n', f) = selectNode (c Map.! child) in
                (n', \(c', e',v') ->
                    Branch n (Map.update (const $ Just $ f (c',e',v')) child c) (v+v') (e+e'))

        confidence :: Float -> Float -> Float -> Float
        confidence v0 v e = e/v + exploration * sqrt (log v0 / v)

selectBest :: (Ord c) => SearchTree a c -> c
selectBest Leaf {} = error "No choices searched yet!"
selectBest Branch {_children=c} =
    maximumBy
        (compare `on`
            (\k -> c Map.! k ^.totalEstimation / c Map.! k ^.totalVisit))
        (Map.keys c)

heuristicSearch :: (Ord c)
    => (a -> [(c, a)])  -- ^ A list of connected vertices, `c` is the edge type.
    -> (a -> Float)  -- ^ An evaluation function.
    -> (a -> Bool)  -- ^ A pruning function. The branch is pruned when this returns False
    -> a -- ^ Starting point
    -> Int  -- ^ Search iteration count.
    -> c
heuristicSearch branches evaluate prune start iter
    = selectBest (iterate' (heuristicSearchIter branches evaluate prune)
        (Leaf start 1 (evaluate start))!!iter)