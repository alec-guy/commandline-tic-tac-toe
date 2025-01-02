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

data TicTacToeGame = TicTacToeGame 
                   {game        :: TicTacToe
                   ,playerShape :: Maybe Mark 
                   }
                   deriving (Eq)
data TicTacToe = TicTacToe
               {rowOne   ::   (Maybe Mark,Maybe Mark,Maybe Mark)
               ,rowTwo   ::   (Maybe Mark,Maybe Mark,Maybe Mark)
               ,rowThree ::   (Maybe Mark,Maybe Mark,Maybe Mark)
               } deriving (Eq)
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
            put (updateGameWithControl control tic)
    maybeWinner <- winnerTestT 
    newtic <- get 
    case maybeWinner of 
        Nothing     -> playGame 
        (Just shape)-> do 
            lift $ putStrLn $ show $ (game newtic)
            lift $ putStrLn $ (show shape) ++ " wins"
chooseShape :: StateT TicTacToeGame IO () 
chooseShape = do 
    lift $ putStr "Enter shape(ex; circle or x): "
    lift $ hFlush stdout 
    shape <- lift $ getLine 
    case shape of 
        "circle" -> do 
            put (TicTacToeGame {game = initialGame, playerShape = Just O})
        "x"      -> do 
            put (TicTacToeGame {game = initialGame, playerShape = Just X})
        _  -> do 
            lift $ putStrLn "Invalid choice, please try again."
            chooseShape 

updateGameWithControl :: Control -> TicTacToeGame -> TicTacToeGame
updateGameWithControl control (TicTacToeGame {game = g, playerShape = js})= 
    case control of 
        (Control (Row r) (Column c)) -> 
            case r of 
                1 -> TicTacToeGame {game = g {rowOne = updateColumn c js (rowOne g)}, playerShape = js}
                2 -> TicTacToeGame {game = g {rowTwo = updateColumn c js (rowTwo g)}, playerShape = js}
                3 -> TicTacToeGame {game = g {rowThree = updateColumn c js (rowThree g)}, playerShape = js}
                _ -> TicTacToeGame {game =g, playerShape = js}
            
updateColumn :: Int8 -> Maybe Mark -> (Maybe Mark, Maybe Mark, Maybe Mark ) -> (Maybe Mark, Maybe Mark, Maybe Mark)
updateColumn num jm (m1,m2,m3) = 
    case num of 
        1 -> (jm,m2,m3)
        2 -> (m1,jm,m3)
        3 -> (m1,m2,jm)
        _  -> (m1,m2,m3)

main :: IO ()
main = do 
    putStrLn "Tic-Tac-Toe Game by Alec"
    evalStateT (chooseShape >> playGame) (TicTacToeGame {game = initialGame, playerShape = Nothing})
    putStrLn "Game over"
