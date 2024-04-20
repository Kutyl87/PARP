-- The germ of a text adventure game
-- Marcin Szlenk 2024
module Main where

introductionText = [
    "You are in the entrance. You notice doors behind you, and you feel pain inside your head. There is a small note at the floor. Maybe you should inspect it?"
    ]

instructionsText = [
    "Available commands are:",
    "",
    "instructions  -- to see these instructions.",
    "go [direction]",
    "Available directions: forward, back, left, right",
    "take [object]",
    "inspect [object]",
    "look",
    "quit          -- to end the game and quit.",
    ""
    ]

-- print strings from list in separate lines
printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)
                  
printIntroduction = printLines introductionText
printInstructions = printLines instructionsText

readCommand :: IO String
readCommand = do
    putStr "> "
    getLine
    
-- note that the game loop may take the game state as
-- an argument, eg. gameLoop :: State -> IO ()
gameLoop :: IO ()
gameLoop = do
    cmd <- readCommand
    case cmd of
        "instructions" -> do printInstructions
                             gameLoop
        "quit" -> return ()
        _ -> do printLines ["Unknown command.", ""]
                gameLoop

main = do
    printIntroduction
    printInstructions
    gameLoop

