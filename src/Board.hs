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
lg f a = unsafePerformIO (print (f a)) `seq` a
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
pieceShape PieceL 0 = listArray ((0,0),(2,2)) [False,True,False,False,True,False,False,True,True]
pieceShape PieceL 1 = listArray ((0,0),(2,2)) [False,False,False,True,True,True,True,False,False]
pieceShape PieceL 2 = listArray ((0,0),(2,2)) [True,True,False,False,True,False,False,True,False]
pieceShape PieceL 3 = listArray ((0,0),(2,2)) [False,False,True,True,True,True,False,False,False]
pieceShape PieceJ 0 = listArray ((0,0),(2,2)) [False,True,True,False,True,False,False,True,False]
pieceShape PieceJ 1 = listArray ((0,0),(2,2)) [False,False,False,True,True,True,False,False,True]
pieceShape PieceJ 2 = listArray ((0,0),(2,2)) [False,True,False,False,True,False,True,True,False]
pieceShape PieceJ 3 = listArray ((0,0),(2,2)) [True,False,False,True,True,True,False,False,False]
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
        PieceL -> listArray ((0,0), (2,2)) [o,x,o,o,x,o,o,x,x]
        PieceJ -> listArray ((0,0), (2,2)) [o,x,x,o,x,o,o,x,o]
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
type FrozenField ia = ia Position Bool

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

showField :: IArray ia Bool => FrozenField ia -> String
showField f = intercalate "\n" $
    [concat [if f!(x,y) then "[]" else "  " | x <- [0..xmax]] | y <- [ymax,ymax-1..0]]
    where
        (xmax, ymax) = snd $ bounds f

showFieldWithActivePiece ::
    IArray ia Bool => FrozenField ia -> Piece -> (Position, Rotation) -> String
showFieldWithActivePiece fb pc st@((x0, y0), rot) =
    intercalate "\n" $
    [concat [showBlock (fb!(x,y)) (locate x y) | x <- [0..xmax]] | y <- [ymax,ymax-1..0]]
    where
        (xmax, ymax) = snd $ bounds fb
        showBlock True _ = "##"
        showBlock False True = "[]"
        showBlock _ _ = "  "
        locate x y =
            let relpos = (x-x0, y-y0) in
            let psh = pieceShape pc rot in
            let bds = bounds psh in
                inRange bds relpos && psh ! relpos

-- TODO make this look better
showBoard :: BoardConstraint ma ia m => Board ma ia m -> m String
showBoard b = do
    fb::Array Position Bool <- freeze (b^.field)
    return $ showFieldWithActivePiece fb (b^.piece) (b^.pieceState)

data Move = MLeft | MRight | MDown | MSoft | MDASLeft | MDASRight | MRLeft | MRRight | MRFlip
    deriving (Eq, Show, Enum, Ord, Ix, Read, Bounded, Generic)

validPosition :: (IArray ia Bool)
              => FrozenField ia  -- frozen field
              -> Piece
              -> (Position, Rotation)
              -> Bool
validPosition f p ((x0, y0),r) = all
    (\(x, y)
        -> not (pieceShape p r ! (x, y)) ||  -- It's fine if the block is not occupied
            inRange (bounds f) (x+x0, y+y0) &&  -- If it is, it can't go out of bound
                not (f ! (x+x0, y+y0)))  -- and it cannot crash with the field
    (indices $ pieceShape p r)


validBoardPosition :: (BoardConstraint ma ia m) => Board ma ia m -> m Bool
validBoardPosition b = do
    frozenf::Array Position Bool <- freeze (b^.field)
    return (validPosition frozenf (b^.piece) (b^.pieceState))

spawnPiece :: BoardConstraint ma ia m => Piece -> (Position, Rotation) -> Board ma ia m -> Board ma ia m
spawnPiece pc s brd = brd
    & piece .~ pc
    & pieceState .~ s

spawnPieceState :: BoardConstraint ma ia m => Piece -> (Position, Rotation) -> StateT (Board ma ia m) m ()
spawnPieceState pc s = do
    piece .= pc
    pieceState .= s

computeMove :: (IArray a Bool, IArray ia Position)
            => FrozenField a  -- frozen field
            -> KickTable ia
            -> Piece
            -> (Position, Rotation)
            -> Move
            -> ((Position, Rotation), Bool)  -- (end state, moved?)
computeMove fb kt pc ((x0, y0), rot) m = do
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
            if validPosition fb pc ((x1, y1), rot) then
                (((x1, y1), rot), True)
            else
                (((x0, y0), rot), False)
        -- tries until fails
        tryMoves (x, y) = pack . pred . head $ filter
            -- we get the first invalid position,
            -- then backtract to get the last consecutive valid one
            -- and pack the thing up into an out-of-the-box format
            (not . validPosition fb pc . (,rot) . try)
            [1..]
            where
                try n = (x0 + x * n, y0 + y * n)
                pack 0 = (((x0, y0), rot), False)
                pack n = ((try n,    rot), True)
        -- rotation, much the same as moving, but we take the first valid kick, instead of the last
        tryRot r = packhead $ filter
            (validPosition fb pc . (,rot') . try)
            [0..maxn]
            where
                maxn = (kt & bounds & snd) ^. _4
                rot' = (rot + r) `mod` 4
                try n = kt !
                    (pc, rot, rot', n) `add` (x0, y0)
                packhead (n:ns) = ((try n, (rot + r) `mod` 4), True)
                packhead [] = (((x0, y0), rot), False)

-- wrapper that packs the data up
computeMoveBoard :: BoardConstraint ma ia m
                 => Board ma ia m
                 -> Move
                 -> m ((Position, Rotation), Bool)
computeMoveBoard br mv = do
    frz::Array Position Bool <- freeze (br^.field)
    return $ computeMove frz (kickTable br) (br^.piece) (br^.pieceState) mv

makeMoveBoard :: BoardConstraint ma ia m => Move -> Board ma ia m -> m (Board ma ia m, Bool)
makeMoveBoard m b = do
    (state, res) <- computeMoveBoard b m
    return (b & pieceState .~ state, res)

-- locks the tetrimino in-place
-- doesn't check for anything
lock :: IArray a Bool => FrozenField a -> Piece -> (Position, Rotation) -> FrozenField a
lock fb pc st@(p0, rot) = fb // [(pos `add` p0, True) |
    let psh = pieceShape pc rot,
    pos <- range $ bounds psh,
    psh ! pos]

lockBoard :: BoardConstraint ma ia m => Board ma ia m -> m ()
lockBoard b = do
    let f = b^.field
    let (x0, y0) = b^.piecePosition
    forM_ (indices psh)  -- for each block in the piece
        (\(x,y)-> when (psh ! (x, y)) $ do
            when (inRange ((0,0), fieldSize b & both %~ pred) (x+x0,y+y0)) $ do
                writeArray f (x+x0,y+y0) True) -- set block
    where psh = pieceShape (b^.piece) (b^.pieceRotation)

lockState :: BoardConstraint ma ia m => StateT (Board ma ia m) m ()
lockState = do
    b <- gets lockBoard
    lift b
    piece .= PieceEmpty

-- Inverts a move, ignoring no-ops. This is used for finesse searching
invertMove :: (IArray a Bool, IArray ia Position)
            => FrozenField a  -- frozen field
            -> KickTable ia
            -> Piece
            -> (Position, Rotation)
            -> Move
            -> [(Position, Rotation)]
invertMove fb kt pc st mv = filter
    -- we filter the candidates by requiring that invertMove is really an inversion
    (\state -> (st, True) == computeMove fb kt pc state mv) $
    _invertMove fb kt pc st mv

-- returns a list of candidates
_invertMove :: (IArray a Bool, IArray ia Position)
            => FrozenField a  -- frozen field
            -> KickTable ia
            -> Piece
            -> (Position, Rotation)
            -> Move
            -> [(Position, Rotation)]
_invertMove fb kt pc ((x0, y0), rot) mv =
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
            [((x1, y1), rot) | validPosition fb pc ((x1, y1), rot)]
        -- checks consecutive moves
        tryMoves (x, y) = map pack $ takeWhile
            (validPosition fb pc . (,rot) . try)
            [1..]
            where
                try n = (x0 - x * n, y0 - y * n)
                pack n = (try n, rot)
        -- rotation, we don't check but returns a list of candidates
        tryRot r = map pack $ filter
            (validPosition fb pc . (,rot) . try)
            [0..maxn]
            where
                rot' = (rot - r) `mod` 4
                maxn = (kt & bounds & snd) ^. _4
                try n = (x0, y0) `sub`
                    (kt ! (pc, rot', rot, n))
                pack n = (try n, rot')

invertMoveBoard  :: BoardConstraint ma ia m
                 => Board ma ia m
                 -> Move
                 -> m [(Position, Rotation)]
invertMoveBoard br mv = do
    frz::Array Position Bool <- freeze (br^.field)
    return $ invertMove frz (kickTable br) (br^.piece) (br^.pieceState) mv

-- Conveniently packs this up into a StateT
moveState :: BoardConstraint ma ia m => Move -> StateT (Board ma ia m) m ()
moveState m = join $ gets (put <=< lift . (fst <$>) . makeMoveBoard m)

detectLines :: (IArray ia Bool) => FrozenField ia -> [Int]
detectLines fb = [ y |
    let (xmax, ymax) = snd $ bounds fb,
    y <- [0..ymax],
    and ([fb!(x,y) | x <- [0..xmax]])]

clearLine :: (IArray ia Bool) => FrozenField ia -> (FrozenField ia, Int)
clearLine f = (ixmap bds (_2 %~ m) f  -- reindex the array, and overwrite the top rows
        // [((x,y),False) | x <- [0..xmax], y <- [1+ymax - length lns..ymax]],
        length lns)
    where
        bds@((0,0), (xmax, ymax)) = bounds f
        lns = detectLines f
        m y = min (estimate y y) ymax
        estimate y0 y | y0-y == length (filter (<=y0) lns) = y0
                      | otherwise = estimate (y+length (filter (<=y0) lns)) y

clearLineBoard :: BoardConstraint ma ia m => Board ma ia m -> m Int
clearLineBoard b = do
    let (xs, ys) = fieldSize b
    let f = b^.field
    fb::Array Position Bool <- freeze f
    let lns = detectLines fb
    forM_ [y0 | y0 <- [0..ys-1], y0 `notElem` lns] (\y ->
        forM_ [0..xs-1] (\x -> do
            block <- readArray f (x,y)
            writeArray f (x,y - length (filter (<y) lns)) block))
    forM_ [ys - length lns..ys-1] (\y ->
        forM_ [0..xs-1] (\x -> writeArray f (x,y) False))
    return (length lns)

clearLineState :: BoardConstraint ma ia m => StateT (Board ma ia m) m Int
clearLineState = do
    b <- gets clearLineBoard
    lift b

isPC :: (IArray ia Bool) => FrozenField ia -> Bool
isPC fb = not $ or $ elems fb

isPCBoard :: BoardConstraint ma ia m => Board ma ia m -> m Bool
isPCBoard b = do
    fb::Array Position Bool <- freeze (b^.field)
    return $ isPC fb

-- I'd really like to see somebody obfuscate this with lens!
add (x, y) (u, v) = (x+u, y+v)
sub (x, y) (u, v) = (x-u, y-v)
