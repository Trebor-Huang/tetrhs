{-# LANGUAGE FlexibleContexts, ConstraintKinds, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
module Board where

import Control.Lens hiding (indices)
import Control.Monad
import Control.Monad.State
import Data.Array.IArray
import Data.Array.MArray
import Data.List
import GHC.Generics
-- A convenient helper function to format and log
-- usage : formatter `lg` object
import System.IO.Unsafe (unsafePerformIO)
--lg f a = unsafePerformIO (print (f a)) `seq` a
--(!?) x y = if inRange (bounds x) y then x!y else error $ "Out of bounds: " ++ show y

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
    deriving (Eq, Show, Enum, Ord, Ix, Read, Bounded, Generic)
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
data Board ma ia (m :: * -> *) = Board {
    _field :: !(ma Position Bool),  -- Bool array of occupation, origin at bottom-left
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

emptyBoard :: BoardConstraint ma ia m => Int -> KickTable ia -> (Int, Int) -> m (Board ma ia m)
emptyBoard spawnLine kickTable fieldSize@(x, y) = do
    arr <- newArray ((0,0), (x-1,y-1)) False
    return Board {
        _field = arr,
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

showField :: IArray ia Bool => ia Position Bool -> String
showField f = intercalate "\n" $
    [concat [if f!(x,y) then "[]" else "  " | x <- [0..xmax]] | y <- [ymax,ymax-1..0]]
    where
        (xmax, ymax) = snd $ bounds f


-- TODO make this look better
showBoard :: BoardConstraint ma ia m => Board ma ia m -> m String
showBoard b = do
    let array = b^.field
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
                inRange bds relpos && psh ! relpos

data Move = MLeft | MRight | MDown | MSoft | MDASLeft | MDASRight | MRLeft | MRRight | MRFlip
    deriving (Eq, Show, Enum, Ord, Ix, Read, Bounded, Generic)

validPosition :: (IArray a Bool)
              => (Int, Int)  -- size
              -> a Position Bool  -- frozen field
              -> Piece
              -> (Position, Rotation)
              -> Bool
validPosition s f p ((x0, y0),r) = all
    (\(x, y)
        -> not (pieceShape p r ! (x, y)) ||  -- It's fine if the block is not occupied
            inRange (bounds f) (x+x0, y+y0) &&  -- If it is, it can't go out of bound
                not (f ! (x+x0, y+y0)))  -- and it cannot crash with the field
    (indices $ pieceShape p r)


validBoardPosition :: forall ma ia m. (BoardConstraint ma ia m) => Board ma ia m -> m Bool
validBoardPosition b = do
    frozenf <- freeze (b^.field) :: m (Array Position Bool)
    return (validPosition (fieldSize b) frozenf (b^.piece) (b^.pieceState))

spawnPiece :: BoardConstraint ma ia m => Piece -> (Position, Rotation) -> Board ma ia m -> Board ma ia m
spawnPiece pc s brd = brd
    & piece .~ pc
    & pieceState .~ s

computeMove :: (IArray a Bool, IArray ia Position)
            => a Position Bool  -- frozen board
            -> (Int, Int) -- Size
            -> KickTable ia
            -> Piece
            -> (Position, Rotation)
            -> Move
            -> ((Position, Rotation), Bool)  -- (end state, moved?)
computeMove fb sz kt pc ((x0, y0), rot) m = do
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

    where
        -- input relative change, output resulting absolute state
        tryMove (x, y) = let (x1, y1) = (x+x0, y+y0) in
            if validPosition sz fb pc ((x1, y1), rot) then
                (((x1, y1), rot), True)
            else
                (((x0, y0), rot), False)
        -- tries until fails
        tryMoves (x, y) = pack . pred . head $ filter
            -- we get the first invalid position,
            -- then backtract to get the last consecutive valid one
            -- and pack the thing up into an out-of-the-box format
            (not . validPosition sz fb pc . (,rot) . try)
            [1..]
            where
                try n = (x0 + x * n, y0 + y * n)
                pack 0 = (((x0, y0), rot), False)
                pack n = ((try n,    rot), True)
        -- rotation, much the same as moving, but we take the first valid kick, instead of the last
        tryRot r = packhead $ filter
            (validPosition sz fb pc . (,rot') . try)
            [0..maxn]
            where
                maxn = (kt & bounds & snd) ^. _4
                rot' = (rot + r) `mod` 4
                try n = kt !
                    (pc, rot, rot', n) `add` (x0, y0)
                packhead (n:ns) = ((try n, (rot + r) `mod` 4), True)
                packhead [] = (((x0, y0), rot), False)

-- wrapper that packs the data up
computeMoveBoard :: forall ma ia m. BoardConstraint ma ia m
                 => Board ma ia m
                 -> Move
                 -> m ((Position, Rotation), Bool)
computeMoveBoard br mv = do
    frz <- freeze (br^.field) :: m (Array Position Bool)
    return $ computeMove frz (fieldSize br) (kickTable br) (br^.piece) (br^.pieceState) mv

makeMoveBoard :: BoardConstraint ma ia m => Move -> Board ma ia m -> m (Board ma ia m, Bool)
makeMoveBoard m b = do
    (state, res) <- computeMoveBoard b m
    return (b & pieceState .~ state, res)

-- locks the tetrimino in-place
-- doesn't check for anything
lock :: BoardConstraint ma ia m => Board ma ia m -> m ()
lock b = do
    let f = b^.field
    let (x0, y0) = b^.piecePosition
    forM_ (indices psh)  -- for each block in the piece
        (\(x,y)-> when (psh ! (x, y)) $ do
            when (inRange ((0,0), fieldSize b & both %~ pred) (x+x0,y+y0)) $ do
                writeArray f (x+x0,y+y0) True) -- set block
    -- ? return $ b & field .~ f
    where psh = pieceShape (b^.piece) (b^.pieceRotation)

lockState :: BoardConstraint ma ia m => StateT (Board ma ia m) m ()
lockState = do
    b <- gets lock
    lift b

-- Inverts a move, ignoring no-ops. This is used for finesse searching
invertMove :: (IArray a Bool, IArray ia Position)
            => a Position Bool  -- frozen board
            -> (Int, Int) -- Size
            -> KickTable ia
            -> Piece
            -> (Position, Rotation)
            -> Move
            -> [(Position, Rotation)]
invertMove fb sz kt pc st mv = filter
    -- we filter the candidates by requiring that invertMove is really an inversion
    (\state -> (st, True) == computeMove fb sz kt pc state mv) $
    _invertMove fb sz kt pc st mv

-- returns a list of candidates
_invertMove :: (IArray a Bool, IArray ia Position)
            => a Position Bool  -- frozen board
            -> (Int, Int) -- Size
            -> KickTable ia
            -> Piece
            -> (Position, Rotation)
            -> Move
            -> [(Position, Rotation)]
_invertMove fb sz kt pc ((x0, y0), rot) mv =
    case mv of
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
        -- ignores no-ops
        tryMove (x, y) = let (x1, y1) = (x-x0, y-y0) in
            [((x1, y1), rot) | validPosition sz fb pc ((x1, y1), rot)]
        -- checks consecutive moves
        tryMoves (x, y) = map pack $ takeWhile
            (validPosition sz fb pc . (,rot) . try)
            [1..]
            where
                try n = (x0 - x * n, y0 - y * n)
                pack n = (try n, rot)
        -- rotation, we don't check but returns a list of candidates
        tryRot r = map pack $ filter
            (validPosition sz fb pc . (,rot) . try)
            [0..maxn]
            where
                rot' = (rot - r) `mod` 4
                maxn = (kt & bounds & snd) ^. _4
                try n = (x0, y0) `sub`
                    (kt ! (pc, rot', rot, n))
                pack n = (try n, rot')

invertMoveBoard  :: forall ma ia m. BoardConstraint ma ia m
                 => Board ma ia m
                 -> Move
                 -> m [(Position, Rotation)]
invertMoveBoard br mv = do
    frz <- freeze (br^.field) :: m (Array Position Bool)
    return $ invertMove frz (fieldSize br) (kickTable br) (br^.piece) (br^.pieceState) mv


-- Conveniently packs this up into a StateT
moveState :: BoardConstraint ma ia m => Move -> StateT (Board ma ia m) m ()
moveState m = join $ gets (put <=< lift . (fst <$>) . makeMoveBoard m)

detectLines :: (IArray ia Bool) => ia Position Bool -> [Int]
detectLines fb = [ x |
    let (xmax, ymax) = snd $ bounds fb,
    x <- [0..xmax],
    and [fb!(x,y) | y <- [0..ymax]]]

clearLine :: forall m ma ia. BoardConstraint ma ia m => Board ma ia m -> m Int
clearLine b = do
    let (xs, ys) = fieldSize b
    let f = b^.field
    fb <- freeze f :: m (Array Position Bool)
    let lns = detectLines fb
    forM_ [y0 | y0 <- [0..ys-1], y0 `notElem` lns] (\y ->
        forM_ [0..xs-1] (\x -> do
            block <- readArray f (x,y)
            writeArray f (x,y - length (filter (<y) lns)) block))
    return (length lns) -- ?

-- I'd really like to see somebody obfuscate this with lens!
add (x, y) (u, v) = (x+u, y+v)
sub (x, y) (u, v) = (x-u, y-v)
