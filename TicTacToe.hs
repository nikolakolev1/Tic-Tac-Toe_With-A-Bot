import Data.Char -- for ord, isUpper, isDigit
import Data.Text.Internal.Builder.Functions (i2d) -- for i2d
import Control.Concurrent
import PickRandom

{- 1. Define appropriate types for a Board, a Move, and an Outcome. -}
-- Token is either X or O
data Token = X | O deriving (Show, Eq)

-- Row, Column and Diagonal are a list of "_" (Nothing), "X", or "O"
type Row      = [Maybe Token]
type Column   = Row
type Diagonal = Row

-- Board is a 3x3 matrix of "_", "X", or "O
data Board = Board [Row] deriving (Show, Eq)

-- Outcome is Toekn X or O if either wins, Draw or Nothing if there is no winner
data Outcome = Win Token | Draw deriving (Show, Eq)

-- Move is: X "A1" or O "B2" (MoveV2 is: X 0 0 or O 1 1)
type Move = (Token, String)
type MoveV2 = (Token, Int, Int)

-- convertMove converts a Move to a MoveV2 ("B1" -> (1, 0))
convertMove :: Move -> MoveV2
convertMove (token, move) = (token, ((read [move!!1]) - 1), ((ord (move!!0)) - 65))


{- 2. Define a function that prints the board in a human-readable format. -}
emptyRow :: Row
emptyRow = [Nothing, Nothing, Nothing]

createStartBoard :: Board
createStartBoard = Board [emptyRow, emptyRow, emptyRow]

printRowMiddle :: Row -> IO ()
printRowMiddle [] = do putStrLn ""
printRowMiddle (tile:rest) = do
    putStr "  "
    case tile of
        Nothing    -> do putStr " "  
        Just token -> do putStr (show token)
    putStr "  |"
    printRowMiddle rest

printEntireRow :: Row -> Int -> IO ()
printEntireRow row index = do
    putStrLn "  |     |     |     |"
    putChar (i2d index) >> putStr" |" >> printRowMiddle row
    putStrLn "  |_____|_____|_____|"

printBoard :: Board -> IO ()
printBoard (Board board) = do
    putStrLn "  ___________________"
    printEntireRow (board!!2) 3
    printEntireRow (board!!1) 2
    printEntireRow (board!!0) 1
    putStrLn "     A     B     C"


{- 3. Define a function that checks whether a move is legal. If it is,
      the function should return the updated Board, otherwise Nothing. -}
-- moveIfValid converts a Move to a MoveV2 and calls moveIfValidHelper
moveIfValid :: Board -> Move -> Maybe Board
moveIfValid (Board board) move = let (token, row, col) = convertMove move in 
    if (row < 0 || row > 2 || col < 0 || col > 2) then
        Nothing
    else if (board!!row!!col /= Nothing) then
        Nothing
    else
        Just (replaceRow (Board board) row (replaceTile (board!!row) col (Just token)))

-- moveAlwayValid takes a board and a move and returns a new board with the move made
moveAlwaysValid :: Board -> MoveV2 -> Board
moveAlwaysValid (Board board) (token, row, col) = replaceRow (Board board) row (replaceTile (board!!row) col (Just token))

-- replaceTile takes a row, a column, and a token and returns a new row with the tile replaced
replaceTile :: Row -> Int -> Maybe Token -> Row
replaceTile row col token = (take col row) ++ [token] ++ (drop (col + 1) row)

-- replaceRow takes a board, a row No, and a new row and returns a new board with the row replaced
replaceRow :: Board -> Int -> Row -> Board
replaceRow (Board board) rowNo newRow = Board ((take rowNo board) ++ [newRow] ++ (drop (rowNo + 1) board))


{- 4. Define a function, which returns if we have a win for player X,
      a win for player O, a draw, or should the game continue? -}
-- checkOutcome returns the winner if there is a one or Nothing if there is no winner
checkOutcome :: Board -> Maybe Outcome
checkOutcome board  = let 
    diagonalsWinner = checkDiagonals [getDiagonal board 1, getDiagonal board 2]
    columnsWinner   = checkColumns [getColumn board 0, getColumn board 1, getColumn board 2]
    rowsWinner      = checkRows [getRow board 0, getRow board 1, getRow board 2]
    in
        if      (diagonalsWinner /= Nothing)    then diagonalsWinner
        else if (columnsWinner /= Nothing) then columnsWinner
        else if (rowsWinner /= Nothing)    then rowsWinner
        else if (length (findAllLegalMoves board X) == 0) then Just Draw
        else Nothing

-- getRow returns a chosen row from the board
getRow :: Board -> Int -> Row
getRow (Board board) row = board!!row

-- checkRow checks if a row has a winner
checkRow :: Row -> Maybe Outcome
checkRow r = 
    if (r!!0 == r!!1 && r!!1 == r!!2) then returnOutcome (r!!0)
    else Nothing

-- checkRows checks if any row has a winner
checkRows :: [Row] -> Maybe Outcome
checkRows [] = Nothing
checkRows (row:rest) = if (checkRow row /= Nothing) then checkRow row else checkRows rest

-- getColumn returns a chosen column from the board
getColumn :: Board -> Int -> Column
getColumn (Board board) col = [board!!0!!col, board!!1!!col, board!!2!!col]

-- checkColumn checks if a column has a winner
checkColumn :: Column -> Maybe Outcome
checkColumn c =
    if (c!!0 == c!!1 && c!!1 == c!!2) then returnOutcome (c!!0)
    else Nothing

-- checkColumns checks if any column has a winner
checkColumns :: [Column] -> Maybe Outcome
checkColumns [] = Nothing
checkColumns (column:rest) = if (checkColumn column /= Nothing) then checkColumn column else checkColumns rest

-- getDiagonal returns a chosen diagonal from the board
getDiagonal :: Board -> Int -> Diagonal
getDiagonal (Board board) 1 = [board!!0!!0, board!!1!!1, board!!2!!2]
getDiagonal (Board board) 2 = [board!!0!!2, board!!1!!1, board!!2!!0]

-- checkDiagonal checks if a diagonal has a winner
checkDiagonal :: Diagonal -> Maybe Outcome
checkDiagonal d = 
    if (d!!0 == d!!1 && d!!1 == d!!2) then returnOutcome (d!!0)
    else Nothing

-- checkDiagonals checks if any diagonal has a winner
checkDiagonals :: [Diagonal] -> Maybe Outcome
checkDiagonals [] = Nothing
checkDiagonals (diagonal:rest) = if (checkDiagonal diagonal /= Nothing) then checkDiagonal diagonal else checkDiagonals rest

-- returnOutcome returns the winner if it is passed a token or Nothing if it is passed Nothing
returnOutcome :: Maybe Token -> Maybe Outcome
returnOutcome (Just X) = Just (Win X)
returnOutcome (Just O) = Just (Win O)
returnOutcome _ = Nothing


{- 5. Define the human player. When a move is requested, the board should be on display,
      and a move should be read from the keyboard. -}
type Player = Token -> Chan Board -> Chan Board -> IO ()

-- humanPlayer takes a token, an input channel, and an output channel and plays the game
humanPlayer :: Player
humanPlayer token input output = do
    board <- readChan input
    printBoard board
    if ((checkOutcome board) /= Nothing) then return ()
    else do
        putStrLn ("Player " ++ show token ++ ", please enter your move: ")
        move <- getLine
        if (length move == 2 && isUpper (move!!0) && isDigit (move!!1) && (moveIfValid board (token, move)) /= Nothing)
            then do
                writeChan output (moveAlwaysValid board (convertMove (token, move)))
                humanPlayer token input output
            else do
                putStrLn "Illegal move, please try again"
                writeChan input board
                humanPlayer token input output


{- 6. Define a bot player. It should make some legal move with an element of randomness,
      unless there is only one “good” move. -}
-- botPlayer takes a token, an input channel, and an output channel and plays the game
botPlayer :: Player
botPlayer token input output = do
    board <- readChan input
    if ((checkOutcome board) /= Nothing) then do return () 
    else do
        move <- pickRandom (findAllLegalMoves board token)
        writeChan output (moveAlwaysValid board (convertMove move))
        botPlayer token input output

-- findAllLegalMoves returns a list of all legal moves
findAllLegalMoves :: Board -> Token -> [Move]
findAllLegalMoves (Board board) token = findAllLegalMovesHelper board token 0 0

-- findAllLegalMovesHelper returns a list of all legal moves
findAllLegalMovesHelper :: [Row] -> Token -> Int -> Int -> [Move]
findAllLegalMovesHelper board token row col =
    if (row == 3) 
        then []
    else if (col == 3) 
        then findAllLegalMovesHelper board token (row + 1) 0
    else if (board!!row!!col == Nothing) 
        then (token, (convertRowColToMove row col)) : findAllLegalMovesHelper board token row (col + 1)
    else 
        findAllLegalMovesHelper board token row (col + 1)

convertRowColToMove :: Int -> Int -> String
convertRowColToMove row col = 
    if (col == 0) then "A" ++ show (row + 1) else
    if (col == 1) then "B" ++ show (row + 1) else
                       "C" ++ show (row + 1)


{- 7. Define the game managing process behaviour. This needs to keep track
      of the board position, the players’ input channels and whose turn it is. -}
-- gameManager takes two input channels and an output channel and manages the game
gameManager :: Chan Board -> Chan Board -> Token -> Chan Board -> Chan Board -> Token-> IO ()
gameManager p1input p1output p1token p2input p2output p2token = do
    board <- readChan p1output              -- read the board from the first player
    if ((checkOutcome board) /= Nothing) then do -- if there is a winner or a draw, end the game
        case (checkOutcome board) of
            Just (Win X) -> gameOver board (Win X)
            Just (Win O) -> gameOver board (Win O)
            Just Draw    -> gameOver board Draw
        return ()
    else do                                      -- if there is no winner or draw, continue the game
        writeChan p2input board             -- write the board to the second player
        board <- readChan p2output          -- read the board from the second player
        if ((checkOutcome board) /= Nothing) then do
            case (checkOutcome board) of
                Just (Win X) -> gameOver board (Win X)
                Just (Win O) -> gameOver board (Win O)
                Just Draw    -> gameOver board Draw
            return ()
        else do
            writeChan p1input board
            gameManager p1input p1output p1token p2input p2output p2token


{- 8. Define a game start that creates the necessary channels, forks the
      player processes, and continue managing the game. Different match-ups
      (humans or bots vs. humans or bots) should be allowed. -}
-- gameStart takes two players and starts the game
gameStart :: Player -> Player -> IO ()
gameStart player1 player2 = do
    p1input  <- newChan
    p1output <- newChan
    p2input  <- newChan
    p2output <- newChan
    forkIO (player1 X p1input p1output)
    forkIO (player2 O p2input p2output)
    writeChan p1input createStartBoard
    gameManager p1input p1output X p2input p2output O

-- gameOver prints the board and the outcome of the game
gameOver :: Board -> Outcome -> IO ()
gameOver board outcome = do
    printBoard board
    putStrLn ("Game Over - " ++ show outcome)


---------------------- TESTS ----------------------
test1 = gameStart humanPlayer botPlayer
test2 = gameStart botPlayer humanPlayer
test3 = gameStart botPlayer botPlayer
test4 = gameStart humanPlayer humanPlayer   