{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Board where

import Data.Array.ST
import Data.Array.IArray
import Data.List
import Control.Lens
import Control.Monad.Loops
import Control.Monad

data Piece
    = PieceEmpty
    | PieceZ
    | PieceS
    | PieceJ
    | PieceL
    | PieceT
    | PieceO
    | PieceI
    -- subject to extension
    deriving (Eq, Show, Enum, Ord, Ix, Read, Bounded)
type Rotation = Int  -- Clockwise

pieceShape :: Piece -> Rotation -> Array (Int, Int) Bool
-- ListArray scans bottom-up, then left-to-right
-- This is hard-coded to speed up. Read the comments for original code
-- TODO Profile this
pieceShape PieceEmpty _ = listArray ((0,0), (0,0)) [False]
pieceShape PieceZ 0 = listArray ((0,0),(2,2)) [False,False,True,False,True,True,False,True,False]
pieceShape PieceZ 1 = listArray ((0,0),(2,2)) [False,False,False,True,True,False,False,True,True]
pieceShape PieceZ 2 = listArray ((0,0),(2,2)) [False,True,False,True,True,False,True,False,False]
pieceShape PieceZ 3 = listArray ((0,0),(2,2)) [True,True,False,False,True,True,False,False,False]
pieceShape PieceS 0 = listArray ((0,0),(2,2)) [False,True,False,False,True,True,False,False,True]
pieceShape PieceS 1 = listArray ((0,0),(2,2)) [False,False,False,False,True,True,True,True,False]
pieceShape PieceS 2 = listArray ((0,0),(2,2)) [True,False,False,True,True,False,False,True,False]
pieceShape PieceS 3 = listArray ((0,0),(2,2)) [False,True,True,True,True,False,False,False,False]
pieceShape PieceJ 0 = listArray ((0,0),(2,2)) [False,True,False,False,True,False,False,True,True]
pieceShape PieceJ 1 = listArray ((0,0),(2,2)) [False,False,False,True,True,True,True,False,False]
pieceShape PieceJ 2 = listArray ((0,0),(2,2)) [True,True,False,False,True,False,False,True,False]
pieceShape PieceJ 3 = listArray ((0,0),(2,2)) [False,False,True,True,True,True,False,False,False]
pieceShape PieceL 0 = listArray ((0,0),(2,2)) [False,True,True,False,True,False,False,True,False]
pieceShape PieceL 1 = listArray ((0,0),(2,2)) [False,False,False,True,True,True,False,False,True]
pieceShape PieceL 2 = listArray ((0,0),(2,2)) [False,True,False,False,True,False,True,True,False]
pieceShape PieceL 3 = listArray ((0,0),(2,2)) [True,False,False,True,True,True,False,False,False]
pieceShape PieceT 0 = listArray ((0,0),(2,2)) [False,True,False,False,True,True,False,True,False]
pieceShape PieceT 1 = listArray ((0,0),(2,2)) [False,False,False,True,True,True,False,True,False]
pieceShape PieceT 2 = listArray ((0,0),(2,2)) [False,True,False,True,True,False,False,True,False]
pieceShape PieceT 3 = listArray ((0,0),(2,2)) [False,True,False,True,True,True,False,False,False]
pieceShape PieceO 0 = listArray ((0,0),(1,1)) [True,True,True,True]
pieceShape PieceO 1 = listArray ((0,0),(1,1)) [True,True,True,True]
pieceShape PieceO 2 = listArray ((0,0),(1,1)) [True,True,True,True]
pieceShape PieceO 3 = listArray ((0,0),(1,1)) [True,True,True,True]
pieceShape PieceI 0 = listArray ((0,0),(3,3)) [False,False,True,False,False,False,True,False,False,False,True,False,False,False,True,False]
pieceShape PieceI 1 = listArray ((0,0),(3,3)) [False,False,False,False,False,False,False,False,True,True,True,True,False,False,False,False]
pieceShape PieceI 2 = listArray ((0,0),(3,3)) [False,True,False,False,False,True,False,False,False,True,False,False,False,True,False,False]
pieceShape PieceI 3 = listArray ((0,0),(3,3)) [False,False,False,False,True,True,True,True,False,False,False,False,False,False,False,False]
pieceShape _ _ = error "Rotation state out of bound. You should `mod` 4 before passing it in."
{-
    pieceShape p 0 = case p of
        PieceZ -> listArray ((0,0), (2,2)) [o,o,x,o,x,x,o,x,o]
        PieceS -> listArray ((0,0), (2,2)) [o,x,o,o,x,x,o,o,x]
        PieceJ -> listArray ((0,0), (2,2)) [o,x,o,o,x,o,o,x,x]
        PieceL -> listArray ((0,0), (2,2)) [o,x,x,o,x,o,o,x,o]
        PieceT -> listArray ((0,0), (2,2)) [o,x,o,o,x,x,o,x,o]
        PieceO -> listArray ((0,0), (1,1)) [x,x,x,x]
        PieceI -> listArray ((0,0), (3,3)) [o,o,x,o,o,o,x,o,o,o,x,o,o,o,x,o]
        where
            x = True
            o = False
    pieceShape p i = rotate (pieceShape p (i-1))
        where
            rotate a =
                let b = snd $ snd $ bounds a in
                    listArray ((0,0), (b,b))
                        [ a ! (b-y, x) | (x,y) <- range ((0,0), (b,b))] -- (b-y, x) -> (x, y)
-}

-- A KickTable is just an array indexed by (piece, starting rotation, ending rotation, test#)
type KickTable a = a (Piece, Rotation, Rotation, Int) (Int, Int)

-- (MArray ma Bool m, IArray ia (Int, Int)) => -- DatatypeContexts is deprecated
data Board ma ia m = Board {
    _field :: !(m (ma (Int, Int) Bool)),  -- Bool array of occupation, origin at bottom-left
    _piece :: !Piece,
    _piecePosition :: (Int, Int),
    _pieceRotation :: Int,
    _holdPiece :: !Piece,
    _nextQueue :: ![Piece], -- Short enough I don't need a queue
    _holdable :: !Bool,
    _pendingAttack :: !Int,
    spawnLine :: !Int, -- Just an approximate indication to compute the current danger
    kickTable :: !(KickTable ia),
    fieldSize :: !(Int, Int)
}
makeLenses ''Board

emptyBoard :: (MArray ma Bool m, IArray ia (Int, Int)) => Int -> KickTable ia -> (Int, Int) -> Board ma ia m
emptyBoard spawnLine kickTable fieldSize@(x, y) = Board {
        _field = newArray ((0,0), (x-1,y-1)) False,
        _piece = PieceEmpty,
        _piecePosition = (0,0),
        _pieceRotation = 0,
        _holdPiece = PieceEmpty,
        _nextQueue = [],
        _holdable = False,
        _pendingAttack = 0,
        spawnLine = spawnLine,
        kickTable = kickTable,
        fieldSize = fieldSize
    }

showBoard :: (MArray ma Bool m, IArray ia (Int, Int)) => Board ma ia m -> m String
showBoard b = do
    array <- b^.field
    let (xmax, ymax) = fieldSize b
    intercalate "\n" <$> mapM sequence
        [[xo <$> readArray array (x, y) <*> return (shadow x y) | x <- [0..xmax-1]] | y <- [ymax-1,ymax-2..0]]
        where
            xo True _ = '*'
            xo False True = 'o'
            xo _ _ = ' '

            shadow x y =
                let relpos = (x-b^.piecePosition._1, y-b^.piecePosition._2) in
                let psh = pieceShape (b^.piece) (b^.pieceRotation) in
                let bds = bounds psh in
                inRange bds relpos && (psh ! relpos)

data Move = MHard | MLeft | MRight | MDown | MSoft | MDASLeft | MDASRight | MRLeft | MRRight | MRFlip | MLock
    deriving (Eq, Show, Enum, Ord, Ix, Read)

validPosition :: (MArray a Bool m) => (Int, Int) -> m (a (Int, Int) Bool) -> Piece -> Rotation -> (Int, Int) -> m Bool
validPosition s b p r (x0, y0) = do
    array <- b
    andM $ map -- for all the blocks in the range below
        (\(x, y) ->
            if not (pieceShape p r ! (x, y)) then  -- It's fine if the block is not occupied
                return True
            else if inRange ((0,0), s & both %~ pred) (x+x0, y+y0) then  -- cannot crash with occupied blocks in field
                not <$> readArray array (x+x0, y+y0)
            else  -- It is out of bounds
                return False)
        (range $ bounds $ pieceShape p r)  -- the range is the bounding box of the tetrimino

validBoardPosition :: (MArray ma Bool m, IArray ia (Int, Int)) => Board ma ia m -> m Bool
validBoardPosition b = validPosition (fieldSize b) (b^.field) (b^.piece) (b^.pieceRotation) (b^.piecePosition)

spawnPiece :: (MArray ma Bool m, IArray ia (Int, Int)) => Piece -> (Int, Int) -> Rotation -> Board ma ia m -> Board ma ia m
spawnPiece pc pos rot brd = brd
    & piece .~ pc
    & piecePosition .~ pos
    & pieceRotation .~ rot

makeMove :: (MArray ma Bool m, IArray ia (Int, Int)) => Move -> Board ma ia m -> m (Board ma ia m, Bool)
makeMove m b = do
    let rot = b^.pieceRotation
    let (x0, y0) = b^.piecePosition
    case m of
        MHard -> do
            (b, mvd) <- tryMods ((piecePosition._2) %~ pred)
            b <- lock b
            return (b, mvd)
        MSoft -> tryMods ((piecePosition._2) %~ pred)
        MLeft -> tryMod $ (piecePosition._1) %~ pred
        MRight -> tryMod $ (piecePosition._1) %~ succ
        MDown -> tryMod $ (piecePosition._2) %~ pred
        MDASLeft -> tryMods ((piecePosition._1) %~ pred)
        MDASRight -> tryMods ((piecePosition._1) %~ succ)
        MRLeft -> tryKick 3
        MRRight -> tryKick 1
        MRFlip -> tryKick 2
        MLock -> do
            b <- lock b
            return (b, True)
    where
        tryMod mod = do -- tries making a move, and tests validity
            let newb = mod b
            valid <- validBoardPosition newb
            if valid then
                return (newb, True)
            else
                return (b, False)

        tryMods mod = do -- tries making a move repeatedly, and tests validity
            iterateBeforeMM
                (fmap not . validBoardPosition . fst)
                (\x -> (mod (fst x), True))
                (b, True)

        tryKick r = do
            let pc = b^.piece
            let startR = b^.pieceRotation
            let endR = (startR + r) `mod` 4
            let kicks =
                    map (kick (pieceRotation %~ ((`mod`4) . (+r))) pc startR endR)
                        $ range $ bounds (kickTable b) &both%~(^._3) -- the length of the kick table
            res <- firstM validBoardPosition kicks
            case res of
                Nothing -> return (b, False)
                Just res -> return (res, True)

        kick mod pc startR endR n =
            let kickpos = kickTable b ! (pc, startR, endR, n) in
                piecePosition %~ move kickpos $ mod b

        move (x, y) (x0, y0) = (x+x0, y+y0)

        -- locks the tetrimino in-place
        -- doesn't check for anything
        lock :: (MArray ma Bool m, IArray ia (Int, Int)) => Board ma ia m -> m (Board ma ia m)
        lock b = do
            f <- b^.field
            let (x0, y0) = b^.piecePosition
            let r = foldM (\brd (x, y) -> if psh ! (x, y) then do
                       if inRange ((0,0), fieldSize b & both %~ pred) (x+x0,y+y0) then do
                           writeArray brd (x+x0,y+y0) True -- set block
                           return brd
                       else
                           return brd
                   else
                       return brd)  -- starting at the original board
                    f
                    (range $ bounds psh)  -- for each block in the piece
            return $ b & field .~ r
            where psh = pieceShape (b^.piece) (b^.pieceRotation)

iterateBeforeMM :: (Monad m) => (a -> m Bool) -> (a -> a) -> a -> m a
iterateBeforeMM p f a = _iterateBeforeMM p f a a
_iterateBeforeMM p f a a' = do
    ok <- p a
    if ok then
        return a'
    else
        _iterateBeforeMM p f (f a) a
