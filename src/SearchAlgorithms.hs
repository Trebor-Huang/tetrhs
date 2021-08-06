{-# LANGUAGE ScopedTypeVariables #-}
module SearchAlgorithms where
-- Standalone module for searching

import Data.List
import Data.Array.ST
import Data.Array
import Control.Monad.ST
import Control.Monad.State (forM_)
import Data.Array.IO
import GHC.IO.Unsafe (unsafePerformIO)

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
memoST r f = (runSTArray compute !)
    where
        compute :: ST s (STArray s a b)
        compute= do
            arr <- newArray_ r
            forM_ (range r) (\i -> do
                writeArray arr i $ f (memoST r f) i)
            return arr

memoArray :: forall a b. (Ix a)
     => (a, a)
     -> ((a -> b) -> a -> b)
     -> a -> b
memoArray r f = (unsafePerformIO compute !)  -- safe!
    where
        compute :: IO (Array a b)
        compute = do
            arr <- newArray_ r :: IO (IOArray a b)
            forM_ (range r) (\i -> do
                writeArray arr i$ f (memoArray r f) i)
            freeze arr
