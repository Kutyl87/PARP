-- The germ of a text adventure game
-- Marcin Szlenk 2024
module Main where

import qualified Items
import qualified Locations
import qualified Game

import Data.List.Split (splitOn)
import Game (GameState(message))
import System.IO (hFlush, stdout)

instructionsText :: [String]
instructionsText = [
    "Available commands are:",
    "",
    "instructions  -- to see these instructions.",
    "go [direction]",
    "Available directions: forward, back, left, right",
    "take [object]",
    "inspect [object]",
    "look",
    "inventory     -- to see items in ypur inventory",
    "quit -- to end the game and quit.",
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

-- note that the game loop may take the game state as
-- an argument, eg. gameLoop :: State -> IO ()
gameLoop :: Game.GameState->IO ()
gameLoop gs = do
    printLines [Game.message gs]
    cmd <- readCommand
    let cmdArgs = splitOn " " cmd
    case head cmdArgs of 
        "instructions" -> do printInstructions
                             gameLoop gs
        "inspect" -> let ngs = Game.describe gs (cmdArgs!!1) in 
                     gameLoop ngs
        "go" -> gameLoop (Game.go gs (cmdArgs!!1))
        "take" -> gameLoop (Game.take gs (cmdArgs!!1))
        "look" -> gameLoop (Game.look gs)
        "describe" -> gameLoop (Game.describe gs (cmdArgs!!1))
        "inventory" -> gameLoop (Game.printInventory gs)
        "quit" -> return ()
        _ -> do gameLoop gs {message = "Unknown command"}

main :: IO ()
main = do
    printInstructions
    let gs = Game.initGameState
    gameLoop gs
