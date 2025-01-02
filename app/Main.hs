{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Trans.State.Strict 
import Control.Applicative
import Control.Monad.Trans.Class

import Data.Maybe (fromJust)
import Data.Int (Int8 (..)) -- small is what we need
import Text.Megaparsec.Error 
import Parser 
import System.IO (hFlush, stdout)

import Data.List (null)
import System.Random 

data TicTacToeGame = TicTacToeGame 
                   {game        :: TicTacToe
                   ,playerShape :: Maybe Mark 
                   ,enemyShape  :: Maybe Mark 
                   }
                   deriving (Eq)
data TicTacToe = TicTacToe
               {rowOne   ::   (Maybe Mark,Maybe Mark,Maybe Mark)
               ,rowTwo   ::   (Maybe Mark,Maybe Mark,Maybe Mark)
               ,rowThree ::   (Maybe Mark,Maybe Mark,Maybe Mark)
               } deriving (Eq)

newtype NPC = NPC Control deriving (Eq)
data SpotStatus = NoSpots | Spots deriving (Eq)

instance Show TicTacToe where 
    show tictac = 
        let f maybemark  = if maybemark == Nothing then " " else show $ fromJust maybemark
            rowone       = case rowOne tictac of 
                            (m1',m2',m3') -> 
                              (box $ f m1') ++ (box $ f m2') ++ (box $ f m3') ++ "\n"
            rowtwo       = case rowTwo tictac of 
                            (m1',m2',m3') -> 
                              (box $ f m1') ++ (box $ f m2') ++ (box $ f m3') ++ "\n"
            rowthree     = case rowThree tictac of 
                            (m1, m2, m3) -> 
                                (box $ f m1) ++ (box $ f m2) ++ (box $ f m3) ++ "\n"
        in rowone ++ rowtwo ++ rowthree
           

box :: String -> String 
box mark = "|" ++ mark ++ "|"

data Mark = O | X deriving (Show, Eq)

tripleFst (x,_,_)   = x 
tripleSnd (_,x,_)   = x 
tripleThird (_,_,x) = x

horizontalTest :: (Maybe Mark,Maybe Mark,Maybe Mark) -> Maybe Mark 
horizontalTest (Just X,Just X,Just X) = Just X 
horizontalTest (Just O,Just O,Just O) = Just O 
horizontalTest _       = Nothing 

horizontalTestT :: StateT TicTacToeGame IO (Maybe Mark)
horizontalTestT  = do 
     tictactoegame <- get 
     let rowOneTest   = horizontalTest $ rowOne $ game tictactoegame 
         rowTwoTest   = horizontalTest $ rowTwo $ game tictactoegame 
         rowThreeTest = horizontalTest $ rowThree $ game tictactoegame
     return (asum [rowOneTest, rowTwoTest, rowThreeTest])
     
verticalTestT :: StateT TicTacToeGame IO (Maybe Mark)
verticalTestT = do 
    tictactoegame <- get 
    let columnOneTest   = horizontalTest ((,,) (tripleFst $ rowOne $ game tictactoegame) (tripleFst $ rowTwo $ game tictactoegame) (tripleFst $ rowThree $ game tictactoegame))
        columnTwoTest   = horizontalTest ((,,) (tripleSnd $ rowOne $ game tictactoegame) (tripleSnd $ rowTwo $ game tictactoegame) (tripleSnd $ rowThree $ game tictactoegame))
        columnThreeTest = horizontalTest ((,,) (tripleThird $ rowOne $ game tictactoegame) (tripleThird $ rowTwo $ game tictactoegame) (tripleThird $ rowThree $ game tictactoegame))
    return (asum [columnOneTest, columnTwoTest, columnThreeTest])

diagonalTestT :: StateT TicTacToeGame IO (Maybe Mark)
diagonalTestT = do 
    tictactoegame <- get 
    let leftDiagonal  = horizontalTest ((,,) (tripleFst $ rowOne $ game tictactoegame) (tripleSnd $ rowTwo $ game tictactoegame) (tripleThird $ rowThree $ game tictactoegame))
        rightDiagnoal = horizontalTest ((,,) (tripleThird $ rowOne $ game tictactoegame) (tripleSnd $ rowTwo $ game tictactoegame) (tripleFst  $ rowThree $ game tictactoegame))
    return (asum [leftDiagonal, rightDiagnoal])

winnerTestT :: StateT TicTacToeGame IO (Maybe Mark)
winnerTestT = do 
    maybewinner  <- horizontalTestT
    maybewinner2 <- verticalTestT
    maybewinner3 <- diagonalTestT
    return (asum [maybewinner, maybewinner2, maybewinner3])

initialGame :: TicTacToe
initialGame = let nada = (Nothing,Nothing,Nothing) in TicTacToe {rowOne = nada, rowTwo = nada, rowThree = nada}

playGame :: StateT TicTacToeGame IO () 
playGame = do 
    tic <- get 
    lift $ putStrLn $ show $ (game tic)
    lift $ putStr "Enter Move: "
    lift $ hFlush stdout 
    move <- lift $ getLine 
    case parseMove move of 
        Left e -> do 
            lift $ putStrLn $ errorBundlePretty e 
            playGame 
        Right control -> 
            put (updateGameWithControl (Left control) tic)
    spotstatus <- playNPC
    case spotstatus of 
        NoSpots -> lift $ putStrLn "Draw" 
        Spots   -> do     
          maybeWinner <- winnerTestT 
          newtic <- get 
          case maybeWinner of 
           Nothing     -> playGame 
           (Just shape)-> do 
              lift $ putStrLn $ show $ (game newtic)
              lift $ putStrLn $ (show shape) ++ " wins"


availableSpots :: (Maybe Mark, Maybe Mark, Maybe Mark) -> [Maybe Int8]
availableSpots (x,y,z) = 
    filter (/= Nothing) ((\t -> if (snd t) == Nothing then Just (fst t) else Nothing ) <$> [(1,x) , (2,y) , (3, z)])

randomElem :: [a] -> IO a 
randomElem list = do 
    s <- newStdGen 
    return ( list !! (fst (randomR (0, (length list) - 1) s)) )

playNPC :: StateT TicTacToeGame IO SpotStatus
playNPC = do 
  tic <- get 
  let rowOneSpots   = fromJust <$> availableSpots (rowOne $ game $ tic) 
      rowTwoSpots   = fromJust <$> availableSpots (rowTwo $ game $ tic)
      rowThreeSpots = fromJust <$> availableSpots (rowThree $ game $ tic)
      allSpots      = filter (/= Nothing) $ 
                       [if null rowOneSpots then Nothing else   Just (1, rowOneSpots) 
                       ,if null rowTwoSpots then Nothing else   Just (2, rowTwoSpots) 
                       ,if null rowThreeSpots then Nothing else Just (3, rowThreeSpots)
                       ] 
      chosenOne     = randomElem (fromJust <$> allSpots)
  case allSpots of 
    [] -> return NoSpots 
    _  -> do 
           (row,columnOptions) <- lift $ chosenOne 
           column              <- lift $ randomElem columnOptions 
           put (updateGameWithControl (Right $ NPC (Control (Row row) (Column column))) tic)
           return Spots 

chooseShape :: StateT TicTacToeGame IO () 
chooseShape = do 
    lift $ putStr "Enter shape(ex; circle or x): "
    lift $ hFlush stdout 
    shape <- lift $ getLine 
    case shape of 
        "circle" -> do 
            put (TicTacToeGame {game = initialGame, playerShape = Just O, enemyShape = Just X})
        "x"      -> do 
            put (TicTacToeGame {game = initialGame, playerShape = Just X, enemyShape = Just O})
        _  -> do 
            lift $ putStrLn "Invalid choice, please try again."
            chooseShape 

updateGameWithControl :: Either Control NPC -> TicTacToeGame -> TicTacToeGame
updateGameWithControl control (TicTacToeGame {game = g, playerShape = js, enemyShape = es})= 
    case control of 
        Left c -> case c of 
                   (Control (Row r) (Column c')) -> 
                       case r of 
                        1 -> TicTacToeGame {game = g {rowOne = updateColumn c' js (rowOne g)}, playerShape = js, enemyShape = es}
                        2 -> TicTacToeGame {game = g {rowTwo = updateColumn c' js (rowTwo g)}, playerShape = js, enemyShape = es}
                        3 -> TicTacToeGame {game = g {rowThree = updateColumn c' js (rowThree g)}, playerShape = js, enemyShape = es}
                        _ -> TicTacToeGame {game =g, playerShape = js, enemyShape = es}
        Right (NPC c') -> case c' of 
                   (Control (Row r) (Column c)) -> 
                       case r of 
                        1 -> TicTacToeGame {game = g {rowOne = updateColumn c es (rowOne g)}, playerShape = js, enemyShape = es}
                        2 -> TicTacToeGame {game = g {rowTwo = updateColumn c es (rowTwo g)}, playerShape = js, enemyShape = es}
                        3 -> TicTacToeGame {game = g {rowThree = updateColumn c es (rowThree g)}, playerShape = js, enemyShape = es}
                        _ -> TicTacToeGame {game =g, playerShape = js, enemyShape = es}
                          
            
updateColumn :: Int8 -> Maybe Mark -> (Maybe Mark, Maybe Mark, Maybe Mark ) -> (Maybe Mark, Maybe Mark, Maybe Mark)
updateColumn num jm (m1,m2,m3) = 
    case num of 
        1 -> 
         if m1 == Nothing then (jm,m2,m3) else (m1,m2,m3)
        2 -> 
         if m2 == Nothing then (m1,jm,m3) else (m1,m2,m3)
        3 -> 
         if m3 == Nothing then (m1,m2,jm) else (m1,m2,m3)
        _  -> (m1,m2,m3)

main :: IO ()
main = do 
    putStrLn "Tic-Tac-Toe Game by Alec"
    evalStateT (chooseShape >> playGame) (TicTacToeGame {game = initialGame, playerShape = Nothing, enemyShape = Nothing})
    putStrLn "Game over"
