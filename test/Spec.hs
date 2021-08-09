{-# LANGUAGE FlexibleContexts, DeriveAnyClass, StandaloneDeriving#-}
import ArrayData
import Board
import Finesse
import PCFinder
import SearchAlgorithms
import Control.Concurrent
import Control.DeepSeq
import Control.Monad.State
import Criterion.Main
import Data.Array.IArray
import Data.Array.IO
import Data.Array.Unboxed
import Data.List
import Data.Map (Map, lookup)
import Data.Maybe

deriving instance (NFData Move)
deriving instance (NFData Piece)

tabulate :: (Ix a) => (a, a) -> String -> (a -> String) -> (a -> String) -> String
tabulate b fname s f = intercalate "\n" [
        fname ++ " " ++ s i ++ " = " ++ f i | i <- range b
    ]

-- ! Test 1
tabulatePieceShape = tabulate ((minBound,0),(maxBound,3)) "pieceShape" (\(p,r) -> show p ++ " " ++ show r) (show . uncurry pieceShape)

testEq :: (Eq a) => a -> a -> IO ()
testEq a b | a == b = putStrLn "Passed."
           | otherwise = error "Unequal."

board :: IO (Board IOArray Array IO)
board = emptyBoard 20 srs (10, 40)
frozenBoard :: Array Position Bool
frozenBoard = listArray ((0,0),(9,39)) (replicate 400 False)

printBoard :: StateT (Board IOArray Array IO) IO ()
printBoard = do
    q <- gets showBoard  -- gets the IO action that accesses the IOArray
    r <- liftIO q      -- Computes the String
    liftIO $ putStrLn r  -- prints the string
    liftIO $ putStrLn "--------------------\x1B[H"
    liftIO $ threadDelay 300000

-- ! Test 2
operate :: IO ((), Board IOArray Array IO)
operate = do
    b <- board
    runStateT (do
        sequence_ $ intersperse printBoard [
                spawnPieceState PieceZ ((3,20), 0),
                moveState MLeft,
                moveState MDown,
                moveState MSoft,
                lockState,
                void clearLineState,
                spawnPieceState PieceT ((5,20), 0),
                moveState MRRight,
                moveState MDASLeft,
                moveState MSoft,
                moveState MRLeft,
                lockState,
                void clearLineState,
                spawnPieceState PieceI ((5,5), 0),
                moveState MSoft,
                lockState,
                void clearLineState,
                spawnPieceState PieceJ ((4,4),2),
                moveState MDASRight,
                moveState MSoft,
                lockState,
                void clearLineState
            ]
        printBoard) b

invCollatz :: [Int] -> [[Int]]
invCollatz ms@(m:_) | m == 4         = [8:ms]
                    | m `mod` 6 == 4 = [m * 2:ms, (m-1) `div` 3:ms]
                    | otherwise      = [m * 2:ms]
invCollatz [] = error "Nothing to start with"

-- ! Test 3
bfsTest = bfs ((==21).head) (const True) [1] invCollatz

-- ! Test 4
weird :: (Int -> Int) -> Int -> Int
weird _ 0 = 0
weird _ 1 = 0
weird f i = f (i - 2) + f (i - 1) + 1

stweird n = memoST (0,n) weird n

-- ! Test 5
invertTest = do
    ((), board) <- operate
    m <- invertMoveBoard board MRLeft
    print m

-- ! Test 6
grid :: Position -> [(Bool, Position)]
grid (x,y) | x > 10 && y > 10 = []
           | x > 10 = [(False, (x,y+1))]
           | y > 10 = [(True, (x+1,y))]
           | otherwise = [(False, (x,y+1)), (True, (x+1,y))]

findRoute = bfsRoute ((0,0), (11,11)) grid (0,0)

-- ! Test 7

searchTest :: [((Position, Rotation), Bool)] -> [Maybe [Move]]
searchTest = map (searchFinesse
        frozenBoard
        ((-2,-2),(9,21))
        (srs :: KickTable Array)
        standardMoves
        PieceZ
        ((4,20),0))

-- ! Test 8

usefulFinesse :: (Piece, Piece) -> [(Piece, (Position, Rotation), [Move])]
usefulFinesse (minBound, maxBound) =
    [ (pc, (pos, rot), moves) |
        pc <- range (minBound, maxBound),
        let srch = searchFinesse frozenBoard ((-2,-2), (9,21)) (srs::KickTable Array) standardMoves
                pc (guideLineSpawnPositions!pc,0),
        pos <- range ((-2,-2),(9,0)),
        rot <- [0,1,2,3],
        validPosition frozenBoard pc (pos, rot),
        let result = srch ((pos, rot), True),
        endsWithSoftDrop result,
        let (Just moves) = result]
    where
        endsWithSoftDrop (Just l) = last l == MSoft
        endsWithSoftDrop _ = False

-- ! Test 9

zspinBoard :: UArray Position Bool
zspinBoard = listArray ((0,0), (9,9))  -- 10x10, rotated
    [x,x,x,o,o,o,o,o,o,o  --  +--> y
    ,x,x,x,o,o,o,o,o,o,o  --  | 
    ,x,x,x,o,o,o,o,o,o,o  --  x
    ,x,x,x,x,o,o,o,o,o,o
    ,o,o,x,o,o,o,o,o,o,o
    ,x,o,o,o,o,o,o,o,o,o
    ,x,x,x,o,o,o,o,o,o,o
    ,x,x,x,o,o,o,o,o,o,o
    ,x,x,x,o,o,o,o,o,o,o
    ,x,x,x,o,o,o,o,o,o,o]
    where
        x = True
        o = False

testAllPlacements :: Piece -> Map ((Position, Rotation), Bool) [Move]
testAllPlacements pc = allPlacements
    zspinBoard
    ((-2,-2),(10,10))
    (srs :: KickTable Array)
    standardMoves
    pc
    ((4,8), 0)

-- ! Test 10

pcBoard3 :: UArray Position Bool
pcBoard3 = listArray ((0,0), (9,9))  -- 10x10, rotated
    [x,x,x,x,o,o,o,o,o,o  --  +--> y
    ,x,x,x,x,o,o,o,o,o,o  --  | 
    ,x,x,x,x,o,o,o,o,o,o  --  x
    ,x,x,x,x,o,o,o,o,o,o
    ,x,x,x,x,o,o,o,o,o,o
    ,x,x,x,x,o,o,o,o,o,o
    ,x,x,x,x,o,o,o,o,o,o
    ,o,o,o,o,o,o,o,o,o,o
    ,o,o,o,o,o,o,o,o,o,o
    ,o,o,o,o,o,o,o,o,o,o]
    where
        x = True
        o = False

pcBoard4 :: UArray Position Bool
pcBoard4 = listArray ((0,0), (9,9))  -- 10x10, rotated
    [x,x,x,x,o,o,o,o,o,o  --  +--> y
    ,x,x,x,x,o,o,o,o,o,o  --  | 
    ,x,x,x,x,o,o,o,o,o,o  --  x
    ,x,x,x,x,o,o,o,o,o,o
    ,x,x,x,x,o,o,o,o,o,o
    ,x,x,x,x,o,o,o,o,o,o
    ,o,o,o,o,o,o,o,o,o,o
    ,o,o,o,o,o,o,o,o,o,o
    ,o,o,o,o,o,o,o,o,o,o
    ,o,o,o,o,o,o,o,o,o,o]
    where
        x = True
        o = False

pcBoard5 :: UArray Position Bool
pcBoard5 = listArray ((0,0), (9,9))  -- 10x10, rotated
    [x,x,x,x,o,o,o,o,o,o  --  +--> y
    ,x,x,x,x,o,o,o,o,o,o  --  | 
    ,x,x,x,x,o,o,o,o,o,o  --  x
    ,x,x,x,x,o,o,o,o,o,o
    ,x,x,x,x,o,o,o,o,o,o
    ,o,o,o,o,o,o,o,o,o,o
    ,o,o,o,o,o,o,o,o,o,o
    ,o,o,o,o,o,o,o,o,o,o
    ,o,o,o,o,o,o,o,o,o,o
    ,o,o,o,o,o,o,o,o,o,o]
    where
        x = True
        o = False

pcTest :: (IArray fa0 Bool) => FrozenField fa0 -> [Piece] -> Maybe [[Move]]
pcTest pcBoard pcs =  listToMaybe $ searchPC
    pcBoard ((-2,-2),(10,10))
    (srs::Array (Piece, Rotation, Rotation, Int) Position) standardMoves
    (guideLineSpawnPositions//[(m, (4,5)) | m <- range (minBound, maxBound)])
    pcs

makeBoard :: FrozenField Array -> IO (Board IOUArray Array IO)
makeBoard fb = do
    b <- thaw fb
    let ((0,0),(x,y)) = bounds fb
    bd <- board
    return bd {_field=b, fieldSize=(x+1,y+1)}

main :: IO ()
main = do
    benchMarks

benchMarks :: IO ()
benchMarks = defaultMain
    [
        bgroup "finesse"
            [
                bench "#1" $ nf searchTest [(((2,0), 1),True),(((2,0),1),False)],
                bench "#2" $ nf usefulFinesse (PieceZ, PieceI)
            ],
        bgroup "moves"
            [
                bench "Z Spin" $ nf testAllPlacements PieceZ,
                bench "No Spin" $ nf testAllPlacements PieceI
            ],
        bgroup "PC"
            [
                bench "3x4#1" $ nf (pcTest pcBoard3) [PieceZ, PieceT, PieceJ],
                bench "3x4#2" $ nf (pcTest pcBoard3) [PieceJ, PieceL, PieceT],
                bench "4x4" $ nf (pcTest pcBoard4) [PieceT, PieceT, PieceJ, PieceL],
                bench "5x4" $ nf (pcTest pcBoard5) [PieceI, PieceT, PieceO, PieceZ, PieceL],
                bench "prune#1" $ nf (pcTest pcBoard4) [PieceJ, PieceL, PieceT],
                bench "prune#2" $ nf (pcTest zspinBoard) [PieceZ]
            ]
    ]
