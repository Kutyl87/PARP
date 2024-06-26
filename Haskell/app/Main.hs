-- The germ of a text adventure game
-- Marcin Szlenk 2024
module Main where

import qualified Items
import qualified Locations
import qualified Game
import qualified Types (Event)

import Data.List.Split (splitOn)
import Types
import System.IO (hFlush, stdout)
import System.Random (randomRIO)

instructionsText :: [String]
instructionsText = [
    "Available commands are:",
    "",
    "instructions  -- to see these instructions.",
    "go [direction]",
    "Available directions: forward, back, left, right",
    "take [object]",
    "inspect [object]",
    "craft [object]",
    "look",
    "inventory     -- to see items in your inventory",
    "rest          -- to regenerate energy",
    "quit          -- to end the game and quit.",
    "buy           -- to buy an item",
    ""
    ]

-- print strings from list in separate lines
printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

printInstructions :: IO ()
printInstructions = printLines instructionsText

readCommand :: IO String
readCommand = do
    putStr "> "
    hFlush stdout
    getLine

joinArgs::[String]->String
joinArgs [] = ""
joinArgs (x:xs) = if null xs then
    x else (x ++ " ") ++ joinArgs xs
    

-- note that the game loop may take the game state as
-- an argument, eg. gameLoop :: State -> IO ()
gameLoop :: Types.GameState->IO ()
gameLoop gs = do
    printLines [Types.message gs]
    if Types.dead gs == True then return ()
    else do
        cmd <- readCommand
        let cmdArgs = splitOn " " cmd
        case head cmdArgs of 
            "instructions" -> do printInstructions
                                 gameLoop gs
            "inspect" -> gameLoop (Game.describe gs (cmdArgs!!1))
            "go" -> gameLoop (Game.go gs (cmdArgs!!1))
            "take" -> gameLoop (Game.take gs (joinArgs (tail cmdArgs)))
            "look" -> gameLoop (Game.look gs)
            "rest" -> gameLoop (Game.rest gs)
            "inventory" -> gameLoop (Game.printInventory gs)
            "ask" -> do 
                int1 <- randomRIO(0, 10)
                int2 <- randomRIO(0,10)
                gameLoop(Game.ratKingRiddle gs int1 int2)
            "yes" -> if Types.currentLocation gs == "Side tunnel" && not (elem Types.RatKingDefeated (Types.events gs)) then 
                    do 
                    int1 <- randomRIO(0, 10)
                    int2 <- randomRIO(0,10)
                    gameLoop (Game.ratKingRiddle gs int1 int2) else gameLoop gs {message="Unknown command"}
            "no" -> if Types.currentLocation gs == "Side tunnel" && not (elem Types.RatKingDefeated (Types.events gs)) then gameLoop (Game.ratKingReject gs) else gameLoop gs {message="Unknown command"}
            "answer" -> if Types.currentLocation gs == "Side tunnel" && not (elem Types.RatKingDefeated (Types.events gs)) then gameLoop (Game.ratKingAnswer gs (cmdArgs!!1))  else gameLoop gs {message="Unknown command"}
            "craft" -> gameLoop (Game.craft gs (joinArgs (tail cmdArgs)))
            "fight" -> gameLoop (Game.fight gs (cmdArgs!!1))
            "buy" -> gameLoop (Game.buy gs (cmdArgs!!1))
            "use" -> gameLoop (Game.use gs (cmdArgs!!1))
            "rest" -> gameLoop (Game.rest gs)
            "quit" -> return ()
            _ -> do gameLoop gs {message = "Unknown command"}

main :: IO ()
main = do
    printInstructions
    let gs = Game.initGameState
    gameLoop gs
