module Items where

import Data.Map (Map, lookup)
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe )

note :: String
note = "note"

stone_tablet_half :: String
stone_tablet_half = "Stone tablet half"

stone_tablet :: String
stone_tablet = "Stone tablet"

recpies :: Map String [(String, Int)]
recpies = Map.fromList [
    (stone_tablet, [(stone_tablet_half, 2)])]

descriptions :: Map String String
descriptions = Map.fromList [
    (note, "You read the note from a lost wanderer. It says: You are in a maze. You need to find a way out. There are 3 tunnels. The first one is very dangerous. The second one has a light at the end - thats the path you should take. The third one may hold a secret. Choose wisely.")]

printItemList::[(String, Int)]->String
printItemList [] = ""
printItemList (x:xs) = fst x ++ " (count: " ++ show (snd x) ++ ")\n" ++ printItemList xs

checkRecipeItems::Data.Map.Map String Int->[(String, Int)]->Bool
checkRecipeItems _ [] = True
checkRecipeItems i (x:xs) = (fromMaybe 0 (Map.lookup (fst x) i) >= snd x ) && checkRecipeItems i xs

subtractRecipeItems::Data.Map.Map String Int->[(String, Int)]->Data.Map.Map String Int
subtractRecipeItems i [] = i
subtractRecipeItems i (x:xs) = let sub y = y-snd x in
    subtractRecipeItems (Map.adjust sub (fst x) i) xs

