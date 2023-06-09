module MultiThread_version where
import Data.Time.Clock.System
import Data.Bits
import Control.Concurrent
import Data.Char
import Data.Typeable
import System.Exit

pickRandom :: [Int] -> IO Int
pickRandom [] = undefined
pickRandom [x] = return x
pickRandom xs = do
   t <- getSystemTime
   let n = systemNanoseconds t
   let nn = fromIntegral(shiftR (n * 123456789) 16) -- some kind of multiplicative hashing
   let m = length xs
   return (xs !! (mod nn m))


-- 1. 

type Board = [Int]
type Move = (Bool , Int)                    -- True: Player 'X'   False: Player 'O'      Int: 0-8 index of Board

data Outcome = Win Bool | Draw 
   deriving Show

initialBoard :: Board 
initialBoard = [0, 0, 0, 
                0, 0, 0, 
                0, 0, 0]
                                                {-
                                                   0th  -    2th    
                                                   -    5th  -    
                                                   6th  -    8th    
                                                -}

-- 2. 

printBoard :: Board -> IO ()
printBoard board = 
   if length board /= 9 
      then error "Invalid Board"
      else do
         putStrLn ""
         printCells (take 3 board)
         printCells (take 3 (drop 3 board))
         printCells (drop 6 board)
         putStrLn "please, type 0-8 corresponding the 9 boxes"


printCells :: Board -> IO ()
printCells [] = putStrLn ""
printCells (cell:cells) = do
    case cell of
        0 -> putStr "-    "
        1 -> putStr "X    "
        2 -> putStr "O    "
        _ -> error "Invalid cell"
    printCells cells

   
-- 3.

isMoveLegal :: Board -> Move -> Maybe Board
isMoveLegal board (player, index)
   | length board /= 9 || index > 8 || index < 0    = Nothing
   | board !! index == 0                            = Just (take index board ++ boolToIntList player ++ drop (index+1) board)
   | otherwise                                      = Nothing

boolToIntList :: Bool -> [Int]
boolToIntList b = if b then [1] else [2]

-- 4.

getOutcome :: Board -> Maybe Outcome
getOutcome []    = Nothing
getOutcome board = 
   if      board !! 0 ==1 && board !! 1 ==1 && board !! 2 ==1 then Just (Win True)
   else if board !! 3 ==1 && board !! 4 ==1 && board !! 5 ==1 then Just (Win True)
   else if board !! 6 ==1 && board !! 7 ==1 && board !! 8 ==1 then Just (Win True)

   else if board !! 0 ==1 && board !! 3 ==1 && board !! 6 ==1 then Just (Win True)
   else if board !! 1 ==1 && board !! 4 ==1 && board !! 7 ==1 then Just (Win True)
   else if board !! 2 ==1 && board !! 5 ==1 && board !! 8 ==1 then Just (Win True)

   else if board !! 0 ==1 && board !! 4 ==1 && board !! 8 ==1 then Just (Win True)
   else if board !! 6 ==1 && board !! 4 ==1 && board !! 2 ==1 then Just (Win True)


   else if board !! 0 ==2 && board !! 1 ==2 && board !! 2 ==2 then Just (Win False)
   else if board !! 3 ==2 && board !! 4 ==2 && board !! 5 ==2 then Just (Win False)
   else if board !! 6 ==2 && board !! 7 ==2 && board !! 8 ==2 then Just (Win False)

   else if board !! 0 ==2 && board !! 3 ==2 && board !! 6 ==2 then Just (Win False)
   else if board !! 1 ==2 && board !! 4 ==2 && board !! 7 ==2 then Just (Win False)
   else if board !! 2 ==2 && board !! 5 ==2 && board !! 8 ==2 then Just (Win False)

   else if board !! 0 ==2 && board !! 4 ==2 && board !! 8 ==2 then Just (Win False)
   else if board !! 6 ==2 && board !! 4 ==2 && board !! 2 ==2 then Just (Win False)

   else if any (== 0) board then Nothing
   else Just Draw


----------------------------------------------------------------

data Msg =  Invitation Board | MoveMsg Move
type Player = (Bool, Chan Msg, Chan Msg)  -- true is 'X'   false is 'O'    and   Input & Output channels

getLegalMoves :: Board -> [Int] -> Int -> [Int]
getLegalMoves []     acc _     =  acc
getLegalMoves (x:xs) acc index =  if x == 0 
                                    then
                                       getLegalMoves xs (acc++[index]) (index+1)
                                    else
                                       getLegalMoves xs acc            (index+1)
----------------------------------------------------------------

-- 5.
humanPlayer :: Player -> IO()
humanPlayer (x_or_o, input_C, output_C) = do
                                             Invitation board <- readChan input_C
                                             printBoard board
                                             num <- getLegalUserInput board
                                             writeChan      output_C    (MoveMsg (x_or_o,num))
                                             humanPlayer    (x_or_o,input_C,output_C)


getLegalUserInput :: Board -> IO Int
getLegalUserInput board = do
   keystroke <- getChar
   let num = ord keystroke - ord '0'
   let moves = getLegalMoves board [] 0 
   if elem num moves
      then return num
      else do
         putStrLn "Invalid or illegal move, please try again, only 0-8"
         getLegalUserInput board
                                    

-- 6.
botPlayer ::  Player -> IO()
botPlayer (x_or_o,input_C,output_C) = do
                                       Invitation board <- readChan input_C
                                       let legalMoves = getLegalMoves board [] 0
                                       --putStrLn $ show legalMoves
                                       chosenIndex <- pickRandom legalMoves
                                       
                                       writeChan output_C (MoveMsg (x_or_o,chosenIndex))
                                       botPlayer (x_or_o,input_C,output_C)


-- 7.
gameManager :: Board -> Bool -> Player -> Player -> IO()
gameManager board whosTurn (roleA, inputA,outputA) (roleB, inputB,outputB) = do
   --printBoard board

   if whosTurn then do
      writeChan inputA $ Invitation board
      MoveMsg move <- readChan outputA
      case isMoveLegal board move of
         Just newBoard -> do
            --printBoard newBoard
            case getOutcome newBoard of
               Just outcome -> do
                  case outcome of
                     Win _ -> putStrLn "PlayerA Won"  >>  exitWith ExitSuccess
                     Draw  -> putStrLn "---Draw---"  >>  exitWith ExitSuccess
               Nothing -> 
                  gameManager newBoard (not whosTurn) (roleA,inputA,outputA) (roleB, inputB,outputB) -- Game continues
         Nothing       -> error "PlayerA is cheating, inputing invalid Move"

   else do
      writeChan inputB $ Invitation board
      MoveMsg move <- readChan outputB
      
      case isMoveLegal board move of
         Just newBoard -> do
            --printBoard newBoard
            case getOutcome newBoard of
               Just outcome -> do
                  case outcome of
                     Win _ -> putStrLn "PlayerB Won"  >>  exitWith ExitSuccess
                     Draw  -> putStrLn "---Draw---"  >>  exitWith ExitSuccess
               Nothing -> 
                  gameManager newBoard (not whosTurn) (roleA,inputA,outputA) (roleB, inputB,outputB) -- Game continues
         Nothing       -> error "PlayerB is cheating, inputing invalid Move"


-- 8.       First Bool is "is_playerA_Bot"       Second Bool is "is_playerB_Bot"    False means Human
gameStart  :: Bool -> Bool -> IO ()
gameStart is_playerA_Bot is_playerB_Bot = do
                                             inputA  <- newChan :: IO (Chan Msg)
                                             outputA <- newChan :: IO (Chan Msg)
                                             let playerA = (True,inputA,outputA)

                                             if is_playerA_Bot 
                                                then do
                                                   forkIO (botPlayer playerA)
                                                   putStrLn "PlayerA - Bot    running"
                                                else do
                                                   forkIO (humanPlayer playerA)
                                                   putStrLn "PlayerA - Human  running"

                                             inputB  <- newChan :: IO (Chan Msg)
                                             outputB <- newChan :: IO (Chan Msg)
                                             let playerB = (False,inputB,outputB) 

                                             if is_playerB_Bot 
                                                then do
                                                   forkIO (botPlayer playerB)
                                                   --putStrLn "PlayerB - Bot    running"
                                                else do
                                                   forkIO (humanPlayer playerB)
                                                   --putStrLn "PlayerB - Human  running"
                                             
                                             forkIO (gameManager initialBoard True playerA playerB)
                                             --putStrLn "gameManager running"
                                             return ()

{-

还没有完成   需要用 Async的 wait 等方法    让3线程同步一下 , 才能 避免卡顿 .  详见Notion笔记 Concurrent IO

:{
do 
input <- newChan :: IO (Chan Msg) 
output <- newChan :: IO (Chan Msg)
writeChan input (Invitation initialBoard)
botPlayer (True,input,output)
:}

:{
do 
input <- newChan :: IO (Chan Msg) 
output <- newChan :: IO (Chan Msg)
writeChan input (Invitation initialBoard)
humanPlayer (True,input,output)
:}



PlayerA - Human  running
PlayerB - Human  running
gameManager rXu n n i ngO
    -    
-    -    -    
-    X ghci>    O    
your turn, type 0-8 corresponding the 9 boxes


gameStart True True

-}
