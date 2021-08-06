{-# LANGUAGE FlexibleContexts, ConstraintKinds, RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Board where

import Control.Lens
import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import Data.Array.IArray
import Data.Array.ST
import Data.List (intercalate)
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)
lg f a = unsafePerformIO (print (f a)) `seq` a

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
type Position = (Int, Int) -- origin at bottom left

pieceShape :: Piece -> Rotation -> Array Position Bool
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

type BoardConstraint ma ia m = (MArray ma Bool m, IArray ia Position)
data Board ma ia m = Board {
    _field :: !(m (ma Position Bool)),  -- Bool array of occupation, origin at bottom-left
    _piece :: !Piece,
    _piecePosition :: Position,
    _pieceRotation :: Rotation,
    _holdPiece :: !Piece,
    _nextQueue :: ![Piece], -- Short enough I don't need a queue
    _holdable :: !Bool,
    _pendingAttack :: !Int,
    spawnLine :: !Int, -- Just an approximate indication to compute the current danger
    kickTable :: !(KickTable ia),
    fieldSize :: !Position
}
makeLenses ''Board

-- TODO improve on this? profile!
pieceState :: (BoardConstraint ma ia m) => Lens (Board ma ia m) (Board ma ia m) (Position, Rotation) (Position, Rotation)
pieceState = lens
    (\b -> (b^.piecePosition, b^.pieceRotation))
    (\b (p, r) -> b & piecePosition .~ p & pieceRotation .~ r)

emptyBoard :: BoardConstraint ma ia m => Int -> KickTable ia -> (Int, Int) -> Board ma ia m
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

showBoard :: BoardConstraint ma ia m => Board ma ia m -> m String
showBoard b = do
    array <- b^.field
    let (xmax, ymax) = fieldSize b
    intercalate "\n" <$> mapM sequence
        [[xo <$> readArray array (x, y) <*> return (shadow x y) | x <- [0..xmax-1]] | y <- [ymax-1,ymax-2..0]]
        where
            xo True _ = '*'
            xo False True = 'o'
            xo _ _ = ' '
            (x0, y0) = b^.piecePosition

            shadow x y =
                let relpos = (x-x0, y-y0) in
                let psh = pieceShape (b^.piece) (b^.pieceRotation) in
                let bds = bounds psh in
                inRange bds relpos && (psh ! relpos)

data Move = MHard | MLeft | MRight | MDown | MSoft | MDASLeft | MDASRight | MRLeft | MRRight | MRFlip
    deriving (Eq, Show, Enum, Ord, Ix, Read)

validPosition :: (MArray a Bool m)
              => (Int, Int)  -- size
              -> m (a Position Bool)  -- field
              -> Piece
              -> Rotation
              -> Position
              -> m Bool
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

validBoardPosition :: BoardConstraint ma ia m => Board ma ia m -> m Bool
validBoardPosition b = validPosition (fieldSize b) (b^.field) (b^.piece) (b^.pieceRotation) (b^.piecePosition)

spawnPiece :: BoardConstraint ma ia m => Piece -> Position -> Rotation -> Board ma ia m -> Board ma ia m
spawnPiece pc pos rot brd = brd
    & piece .~ pc
    & piecePosition .~ pos
    & pieceRotation .~ rot

computeMove :: BoardConstraint ma ia m => Move -> Board ma ia m -> m ((Position, Rotation), Bool)
computeMove m b = do
    case m of
      MHard -> tryMoves (0,-1)
      MLeft -> tryMove (-1,0)
      MRight -> tryMove (1,0)
      MDown -> tryMove (0,-1)
      MSoft -> tryMoves (0,-1)
      MDASLeft -> tryMoves (-1,0)
      MDASRight -> tryMoves (1,0)
      MRLeft -> tryRot 3
      MRRight -> tryRot 1
      MRFlip -> tryRot 2

    where
        rot = b^.pieceRotation
        (x0, y0) = b^.piecePosition
        -- input relative change, output resulting absolute state
        tryMove (x, y) = do
            let (x1, y1) = (x+x0, y+y0)
            let b' = b & piecePosition .~ (x1, y1)
            valid <- validBoardPosition b'
            if valid then
                return (((x1, y1), rot), True)
            else
                return (((x0, y0), rot), False)
        -- tries until fails
        tryMoves (x, y) = pack . pred . fromJust <$> firstM
            -- after getting the first (head) try that fails,
            -- (NB: firstM serves as the monadic version of head $ filter,
            -- since filterM consumes all the input before handing the result to (head <$>))
            -- we minus one to get the last consecutive try that succeeds
            -- and wrap that up into an out-of-the-box format
            ((not <$>) . validBoardPosition . (b &) . (piecePosition .~) . try)
            -- read that from right to left:
            --   input n
            --   compute the coordinate of the n-th try
            --   a setter of piecePosition to that value
            --   sets the value of the board b
            --   see if the result is a valid position
            --   invert that since we want the first invalid one
            [1..]
            where
                try n = (x0 + x * n, y0 + y * n)
                pack 0 = (((x0, y0), rot), False)
                pack n = ((try n,    rot), True)
        -- rotation, much the same as moving, but we take the first valid kick, instead of the last
        tryRot r = let b' = b & pieceRotation .~ ((rot + r) `mod` 4) in
            pack <$> filterM
            (validBoardPosition . (b' &) . (piecePosition .~) . try)
            [0..maxn]
            where
                maxn = (b & kickTable & bounds & snd) ^. _4
                try n = (b & kickTable) !
                    (b^.piece, rot, (rot + r) `mod` 4, n) `add` (x0, y0)
                pack (n:ns) = ((try n, (rot + r) `mod` 4), True)
                pack [] = (((x0, y0), rot), False)

makeMove :: BoardConstraint ma ia m => Move -> Board ma ia m -> m (Board ma ia m, Bool)
makeMove m b = do
    (state, res) <- computeMove m b
    -- update the board
    let b' = b & pieceState .~ state
    -- in two cases we lock the tetrimino piece
    b'' <- case m of
        MHard -> lock b'
        _ -> return b'
    return (b'', res)

-- locks the tetrimino in-place
-- doesn't check for anything
lock :: BoardConstraint ma ia m => Board ma ia m -> m (Board ma ia m)
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

-- Inverts a move, ignoring no-ops. This is used for finesse searching
invertMove :: BoardConstraint ma ia m => Move -> Board ma ia m -> m [(Position, Rotation)]
invertMove m b = do
    candidates <- _invertMove m b
    filterM (\state ->
        ((b^.pieceState, True) ==) <$>  -- it should get the original state back
            computeMove m  -- when you make a normal move to cancel the inverted move
                (b & pieceState .~ state)) candidates

_invertMove :: BoardConstraint ma ia m => Move -> Board ma ia m -> m [(Position, Rotation)]
_invertMove m b = do
    case m of
      MLeft -> tryMove (-1,0)
      MRight -> tryMove (1,0)
      MDown -> tryMove (0,-1)
      MSoft -> tryMoves (0,-1)
      MDASLeft -> tryMoves (-1,0)
      MDASRight -> tryMoves (1,0)
      MRLeft -> tryRot 3
      MRRight -> tryRot 1
      MRFlip -> tryRot 2
      m -> error $ "It doesn't make sense to invert " ++ show m

    where
        rot = b^.pieceRotation
        (x0, y0) = b^.piecePosition
        -- input relative change, output resulting absolute state
        tryMove (x, y) = do
            let (x1, y1) = (x0-x,y0-y)
            let b' = b & piecePosition .~ (x1, y1)
            valid <- validBoardPosition b'
            if valid then
                return [((x1, y1), rot)]
            else
                return []
        -- generates all the possibilities
        tryMoves (x, y) = map pack <$> takeWhileM
            (validBoardPosition . (b &) . (piecePosition .~) . try)
            [1..]
            where
                try n = (x0 - x * n, y0 - y * n)
                pack n = (try n, rot)
        -- rotation, we don't check but returns a list of candidates
        tryRot r = let b' = b & pieceRotation .~ ((rot - r) `mod` 4) in
            map pack <$> filterM
            (validBoardPosition . (b' &) . (piecePosition .~) . try)
            [0..maxn]
            where
                maxn = (b & kickTable & bounds & snd) ^. _4
                try n = (x0, y0) `sub` ((b & kickTable) !
                    (b^.piece, (rot - r) `mod` 4, rot, n))
                pack n = (try n, (rot - r) `mod` 4)

moveState :: BoardConstraint ma ia m => Move -> StateT (Board ma ia m) m ()
moveState m = join $ gets (put <=< lift . (fst <$>) . makeMove m)

-- I'd really like to see somebody obfuscate this with lens!
add (x, y) (u, v) = (x+u, y+v)
sub (x, y) (u, v) = (x-u, y-v)
