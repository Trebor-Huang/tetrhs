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
import Data.Array
import Data.Array.IO
import Data.Array.ST
import Data.Maybe
import Data.STRef
import Data.Function
import Control.Lens hiding (children)
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

data SearchTree s a c =
    Leaf {_node :: !a, _estimations :: STRef s (Float, Float)} |
    Branch {_node :: !a, _children :: Map.Map c (STRef s (SearchTree s a c)), _estimations :: STRef s (Float, Float)}
    deriving (Generic)
makeLenses ''SearchTree

mctsIterate :: forall a c s. (Ord c)
    => Float -- ^ Exploration parameter
    -> (a -> [(c, a)])  -- ^ Branching
    -> (a -> Bool)  -- ^ Pruning
    -> (a -> Float)  -- ^ Evaluation
    -> STRef s (SearchTree s a c) -> ST s ()
mctsIterate parameter branch prune evaluate tree
    = do
        (nodes, leaf, leafvalue) <- choose tree
        subtrees <- mapM
                (\(c,a) -> do
                    let e = evaluate a
                    eref <- newSTRef (e,1)
                    tref <- newSTRef (Leaf a eref)
                    return (c, tref, e))
                $ filter (prune . snd) $ branch leafvalue
        writeSTRef leaf (Branch {
            _node=leafvalue,
            _children=Map.fromList [ (cval, tref) | (cval, tref, _) <- subtrees],
            _estimations=last nodes})
        forM_ nodes (\x -> do
            modifySTRef' x (\(e,v) -> (e+sum(map (^._3) subtrees), v+int2Float(length subtrees))))
    where
        choose :: STRef s (SearchTree s a c) -> ST s ([STRef s (Float, Float)], STRef s (SearchTree s a c), a)
        choose treeref = do
            tree <- readSTRef treeref
            case tree of
                Leaf {_node=a, _estimations=e} -> return ([e], treeref, a)
                b@Branch {_children=c, _estimations=e} -> do
                    let keys = Map.keys c
                    total <- expTotal b keys
                    Just promisingChild <- maximumByM (compare `onM` ucb b total) keys
                    let subtree = c Map.! promisingChild
                    (choices, leaf, a') <- choose subtree
                    return (e:choices, leaf, a')

        ucb parent total move = do
            (_, v0) <- readSTRef $ parent^.estimations
            child <- readSTRef $ (parent^.children) Map.! move
            (e, v) <- readSTRef $ child^.estimations
            return $ exp(e/v)/total + parameter * sqrt(log v0 / v)

        expTotal parent keys = sum <$> forM keys (\k -> do
            child <- readSTRef $ (parent^.children) Map.! k
            (e,v) <- readSTRef $ child^.estimations
            return $ exp(e/v))

chooseMove :: (Ord c) => SearchTree s a c -> ST s (Maybe c)
chooseMove Leaf {} = return Nothing
chooseMove Branch {_children=ch} = maximumByM (compare `onM` est) (Map.keys ch)
    where
        est c = do
            child <- readSTRef $ ch Map.! c
            (e, v) <- readSTRef $ child^.estimations
            return $ e/v

makeMove :: (Ord c) => c -> SearchTree s a c -> ST s (SearchTree s a c)
makeMove c Leaf {} = error "No moves available."
makeMove c Branch {_children=ch} = do -- TODO benchmark this
    case Map.lookup c ch of
      Nothing -> error "No such move."
      Just subtree -> readSTRef subtree

-- ｜ on, but with monads
onM f g x y = f <$> g x <*> g y