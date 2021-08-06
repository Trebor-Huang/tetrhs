import Board
import ArrayData
import SearchAlgorithms
import Data.Array.IO
import Data.Array.IArray
import Data.List
import Data.Time
import Control.Lens
import Control.Monad.State
import Control.Concurrent

tabulate :: (Ix a) => (a, a) -> String -> (a -> String) -> (a -> String) -> String
tabulate b fname s f = intercalate "\n" [
        fname ++ " " ++ s i ++ " = " ++ f i | i <- range b
    ]

-- ! Test 1
tabulatePieceShape = tabulate ((minBound,0),(maxBound,3)) "pieceShape" (\(p,r) -> show p ++ " " ++ show r) (show . uncurry pieceShape)

testEq :: (Eq a) => a -> a -> IO ()
testEq a b | a == b = putStrLn "Passed."
           | otherwise = error "Unequal."

board :: Board IOArray Array IO
board = emptyBoard 20 srs (10, 40)

printBoard :: StateT (Board IOArray Array IO) IO ()
printBoard = do
    q <- gets showBoard  -- gets the IO action that accesses the IOArray
    r <- liftIO q      -- Computes the String
    liftIO $ putStrLn r  -- prints the string
    liftIO $ putStrLn "----------------"
    liftIO $ threadDelay 500000

-- ! Test 2
operate :: IO ((), Board IOArray Array IO)
operate = runStateT (do
    sequence_ $ intersperse printBoard [
            id %= spawnPiece PieceZ (3,20) 0,
            moveState MLeft,
            moveState MDown,
            moveState MHard,
            id %= spawnPiece PieceT (5,20) 0,
            moveState MRRight,
            moveState MDASLeft,
            moveState MSoft,
            moveState MRLeft
        ]
    printBoard) board

invCollatz :: [Int] -> [[Int]]
invCollatz ms@(m:_) | m == 4         = [8:ms]
                    | m `mod` 6 == 4 = [m * 2:ms, (m-1) `div` 3:ms]
                    | otherwise      = [m * 2:ms]
invCollatz [] = error "Nothing to start with"

-- ! Test 3
bfsTest = bfs ((==21).head) [1] invCollatz
dfsTest = dfs ((==16).head) [1] invCollatz

-- ! Test 4
weird :: (Int -> Int) -> Int -> Int
weird _ 0 = 0
weird _ 1 = 0
weird f i = f (i - 2) + f (i - 1) + 1

naiveweird = weird naiveweird
stweird n = memoST (0,n) weird n
memoweird n = memo (0,n) weird n
arrayweird n = memoArray (0,n) weird n

main :: IO ()
main = do
    ((), board) <- operate
    m <- invertMove MRLeft board
    print m
    return ()
