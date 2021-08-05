import Board
import ArrayData
import Data.Array.IO
import Data.Array.IArray
import Data.List
import Control.Monad.State
import Control.Concurrent

tabulate :: (Ix a) => (a, a) -> String -> (a -> String) -> (a -> String) -> String
tabulate b fname s f = intercalate "\n" [
        fname ++ " " ++ s i ++ " = " ++ f i | i <- range b
    ]

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
    -- liftIO $ threadDelay 1000000
    liftIO $ putStr "\n\n\n\n\n\n"

moveState :: Move -> StateT (Board IOArray Array IO) IO ()
moveState m = join $ gets (put <=< liftIO . (fst <$>) . makeMove m)

modState :: (Monad m) => (s -> s) -> StateT s m ()
modState = put <=< gets

operate :: IO ((), Board IOArray Array IO)
operate = runStateT (do
    sequence_ $ intersperse (return ()) $ join $ replicate 10 [
            modState $ spawnPiece PieceZ (3,20) 0,
            moveState MLeft,
            moveState MDown,
            moveState MHard,
            modState $ spawnPiece PieceT (5,20) 0,
            moveState MRRight,
            moveState MDASLeft,
            moveState MSoft,
            moveState MRLeft,
            moveState MHard
        ]
    printBoard) board

main :: IO ()
main = do
    operate
    return ()
