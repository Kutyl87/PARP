-- The germ of a text adventure game
-- Marcin Szlenk 2024
module Main where

import qualified Items
import qualified Locations
import qualified Game

import Data.List.Split (splitOn)

introductionText :: [String]
introductionText = [
    "You are in the entrance. You notice doors behind you, and you feel pain inside your head. There is a small note at the floor. Maybe you should inspect it?"
    ]

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
    "quit -- to end the game and quit.",
    ""
    ]

-- print strings from list in separate lines
printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)
                  
printIntroduction :: IO ()
printIntroduction = printLines introductionText
printInstructions :: IO ()
printInstructions = printLines instructionsText

readCommand :: IO String
readCommand = do
    putStr "> "
    getLine

-- note that the game loop may take the game state as
-- an argument, eg. gameLoop :: State -> IO ()
gameLoop :: Game.GameState->IO ()
gameLoop gs = do
    print (Game.message gs)
    cmd <- readCommand
    let cmdArgs = splitOn " " cmd
    case head cmdArgs of 
        "instructions" -> do printInstructions
                             gameLoop gs
        "inspect" -> let ngs = Game.describe gs (cmdArgs!!1) in 
                     gameLoop ngs
        "quit" -> return ()
        _ -> do printLines ["Unknown command.", ""]
                gameLoop gs

main :: IO ()
main = do
    printIntroduction
    printInstructions
    let gs = Game.initGameState
    gameLoop gs

-- funkcje zwracają stringi, funkcja głowna wypisuje