module Main where

import System.IO
import System.Random (mkStdGen, StdGen, randomR)
import Control.Monad.State (State, get, put, runState)
import Data.Map.Strict (Map, fromList, adjust, elems)

data GameState = 
    MkGameState {
        players :: [Player]
    ,   cards :: [Card]
    ,   randomGen :: StdGen
    ,   rolls :: Int
    ,   histogram :: Map Int Int
    } deriving Show

data Player = Dog | Hat | Boat | Car | Thimble | Iron deriving Show

data Card = Happy | Sad | Cry | Laugh | Angel | Devil deriving Show

newGame :: Int -> GameState
newGame seed = MkGameState { players = [Dog,Car], cards = [Sad, Cry, Devil, Devil], randomGen = mkStdGen seed, rolls = 0, histogram = fromList [(k,0) | k <- [2..12]]}

roll :: State GameState (Int,Int)
roll = do
    curr <- get
    let gen = randomGen curr
    let (d1, gen') = randomR (1,6) gen
    let (d2, gen'') = randomR (1,6) gen'
    let hist = histogram curr
    let nhist = adjust (1 +) (d1+d2) hist
    put (curr {randomGen = gen'', rolls = rolls curr + 1, histogram = nhist})
    return (d1,d2)
    

printGame :: GameState -> IO ()
printGame gs = do
    putStrLn $ "Players:   " ++ show (players gs)
    putStrLn $ "Cards:     " ++ show (cards gs)
    putStrLn $ "Rolls:     " ++ show (rolls gs)
    putStrLn $ "Histogram: " ++ show (histogram gs)

type TurnFn = GameState -> ((Int,Int), GameState)

-- ETA reduced
turn :: TurnFn
turn = runState roll

multiturn :: Int -> GameState -> TurnFn -> ([(Int,Int)], GameState)
multiturn n gs f =
    let (res, state) = multiturn_ n gs f []
    in (reverse res, state)

multiturn_ :: Int -> GameState -> TurnFn -> [(Int,Int)] -> ([(Int,Int)], GameState)
multiturn_ 0 gs _ acc = (acc,gs)
multiturn_ n gs f acc =
    let (res,gs') = f gs
    in multiturn_ (n - 1) gs' f (res:acc)

showHistogram :: Map Int Int -> IO ()
showHistogram m = do
    let vals = elems m
    let mx = maximum vals
    let tot = sum vals
    putStrLn $ "\nHistogram has " ++ show (length vals) ++ " elements"
    putStrLn $ "Max value is " ++ show mx ++ " of " ++ show tot ++ " turns\n"
    let rows = 10
    mapM_ putStrLn [ [if v >= (mx * (rows - row) `div` rows) then '*' else ' ' | v <- vals ] | row <- [0..rows] ]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "Please enter a numeric seed: "
    seed <- readLn
    let game = newGame seed

    putStr "How many turns: "
    turns <- readLn

    -- printGame game

    let (_, finalGameState) = multiturn turns game turn

    -- putStr "Dice rolls: "
    -- print result

    showHistogram $ histogram finalGameState

    printGame finalGameState
    return ()