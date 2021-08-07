{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module SearchAlgorithms where
-- Standalone module for searching

import Control.Monad.ST
import Control.Monad.State
import Data.Array
import Data.Array.IO
import Data.Array.ST
import Data.List
import Data.STRef
import Control.Monad.Loops
import Data.Maybe

bfs :: (a -> Bool)
    -> a
    -> (a -> [a])
    -> [a]
bfs predicate a0 branch = filter predicate searchspace
    where
        searchspace = a0 : (branch =<< searchspace)

dfs :: (a -> Bool)
    -> a
    -> (a -> [a])
    -> [a]
dfs predicate a0 branch = filter predicate searchspace
    where
        searchspace = a0 : concat (transpose $ branch <$> searchspace)
        -- the only different thing here is we transpose

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
bfsRoute :: forall a c. (Ix a)
    => (a, a)  -- on a range of vertices of type a
    -> (a -> [(c, a)])  -- shows a list of connected vertices, c is the edge type
    -> a -> a -> Maybe [c] -- finds a shortest route between two points
bfsRoute (!r) f (!a) = trace (Just [])
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
                                      Just c' -> return Nothing
                                        ))
                    writeSTRef neighref (catMaybes $ join new_neighbours))
            -- now we get a array of (previous node, last-step-taken)
            -- we just need to follow it to build up a complete route
            return array
        trace route a1 | a1 == a = route
                       | otherwise = do
                            (a',c) <- unpack a1
                            rt <- route
                            trace (Just (c:rt)) a'
