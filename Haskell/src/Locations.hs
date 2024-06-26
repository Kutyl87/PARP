module Locations where 

import Data.Map (Map, fromList, toList, lookup, empty, member, delete, insert)
import Data.Maybe (isNothing, fromMaybe, fromJust, maybe)
import Items
import Types

strToDir::String->Maybe Types.Direction
strToDir s | s == "forward" = Just Types.Forward
        | s == "back" = Just Types.Back
        | s == "left" = Just Types.Left
        | s == "right" = Just Types.Right
        | otherwise = Nothing

getLocationStringAtDir::Types.Location->Types.Direction->Maybe String
getLocationStringAtDir l d = Data.Map.lookup d (paths l)

listItems::Types.Location->String
listItems l = Items.printItemList (Data.Map.toList (Types.items l))


entrance :: Types.Location
entrance = Types.Location
    "Entrance"
    (const "You are in the entrance. You notice doors behind you, and you feel pain inside your head. There is a small note at the floor. Maybe you should inspect it?")
    (Data.Map.fromList [(Items.note, 1)])
    (Data.Map.fromList [(Types.Forward, "Hall")])

hall :: Types.Location
hall = Types.Location
    "Hall"
    (const "There is a hall with 3 tunnels. There must be a way to get out of there.")
    Data.Map.empty
    (Data.Map.fromList [(Types.Back, "Entrance"), (Types.Right, "In front of first tunnel"), (Types.Forward, "In front of second tunnel"), (Types.Left, "In front of third tunnel")])

in_front_of_first_tunnel :: Types.Location
in_front_of_first_tunnel = Types.Location
    "In front of first tunnel"
    (const "You are standing in front of the rightmost tunnel.")
    Data.Map.empty
    (Data.Map.fromList [(Types.Back, "Hall"), (Types.Forward, "First tunnel"), (Types.Left, "In front of second tunnel")])

in_front_of_second_tunnel :: Types.Location
in_front_of_second_tunnel = Types.Location
    "In front of second tunnel"
    (const "You are standing in front of the middle tunnel.")
    Data.Map.empty
    (Data.Map.fromList [(Types.Back, "Hall"), (Types.Forward, "Second tunnel"), (Types.Right, "In front of first tunnel"), (Types.Left, "In front of third tunnel")])

in_front_of_third_tunnel :: Types.Location
in_front_of_third_tunnel = Types.Location
    "In front of third tunnel"
    (const "You are standing in front of the leftmost tunnel.")
    Data.Map.empty
    (Data.Map.fromList [(Types.Back, "Hall"), (Types.Forward, "Third tunnel"), (Types.Right, "In front of second tunnel")])

first_tunnel :: Types.Location
first_tunnel = Types.Location
    "First tunnel"
    (const "You are in the first tunnel. You can smell blood inside. It seems to be a bad way to escape.")
    Data.Map.empty
    (Data.Map.fromList [(Types.Back, "In front of first tunnel"), (Types.Forward, "Dealer room")])

dealer_room :: Types.Location
dealer_room = Types.Location
    "Dealer room"
    dealerDescription
    Data.Map.empty
    (Data.Map.fromList [(Types.Back, "First tunnel"), (Types.Forward, "Aligator room")])
dealerDescription :: Types.GameState -> String
dealerDescription gs = 
    if Items.flute `Data.Map.member` (Types.inventory gs) 
    then "Shalom! You already have the magic flute."
    else "You have entered a Jewish dealer's space. He wants to sell you a magic flute, but he has not specified its purpose. Maybe it can be useful? He wants to help him, it will cost you 50 energy. (use buy(flute) to buy the flute)"

aligator_room = Types.Location
    "Aligator room"
    aligatorDescription
    (Data.Map.fromList [(Items.aligator, 1)])
    (Data.Map.fromList [(Types.Back, "Dealer room"), (Types.Forward, "End of first tunnel")])

aligatorDescription :: Types.GameState -> String
aligatorDescription gs = 
    if isAligatorInLocation "Aligator room" (Types.locations gs) 
    then "You have entered an Aligator space. There is a huge reptile at the back. Fight could be difficult and demanding. Would you try? (Type fight(aligator) to fight)"
    else "The room where you found the aligator is now empty."

end_of_first_tunnel :: Types.Location
end_of_first_tunnel = Types.Location
    "End of first tunnel"
    (const "The sound of water crashing forcefully ahead fills the air. Advancing might prove to be unwise.")
    Data.Map.empty
    (Data.Map.fromList [(Types.Back, "Aligator room"), (Types.Forward, "Waterfall")])

waterfall :: Types.Location
waterfall = Types.Location
    "Waterfall"
    (const "You have entered a waterfall. You can see a light at the end of the tunnel. You are carried away by the current of water.")
    Data.Map.empty
    (Data.Map.fromList [(Types.Back, "End of first tunnel")])

second_tunnel :: Types.Location
second_tunnel = Types.Location
    "Second tunnel"
    (const "You are in the second tunnel. You can see a light at the end of a tunnel. Maybe this is a way to escape?")
    Data.Map.empty
    (Data.Map.fromList [(Types.Back, "In front of second tunnel"), (Types.Forward, "Tunnel diggers")])

tunnel_diggers :: Types.Location
tunnel_diggers = Types.Location
    "Tunnel diggers"
    tunnelDiggersDescription
    Data.Map.empty
    (Data.Map.fromList [(Types.Back, "Second tunnel"), (Types.Forward, "Second tunnel 1"), (Types.Left, "Side tunnel")])

tunnelDiggersDescription :: Types.GameState -> String
tunnelDiggersDescription gs = do
    if (fromMaybe 0 (Data.Map.lookup (Items.stone_tablet_half) (Types.inventory gs)) == 1 ) && (elem Types.RatKingDefeated (Types.events gs)) then "The miners happily greet you. They offer you a broken half of a stone tablet as thanks. You can feel magical energy from it.\nWould you like to take the tablet? (Use take stone_tablet_half to take it)"
    else if (elem Types.RatKingDefeated (Types.events gs)) then "You stand at the entrance to the side tunnel. The miners have moved back into the side tunnel to keep digging."
    else "You see a few people with mining tools. They tell you that they're trying to dig a tunnel to the surface but a herd of rats stands in their way. They point at a crudely built tunnel entrance to the left. Prehaps they will help you if you slay the rats?"

side_tunnel :: Types.Location
side_tunnel = Types.Location
    "Side tunnel"
    (const "You are in a narrow, crudely built tunnel. Deep inside you can see the towering shadow of a giant rat. \
    \Greeting, traveler. What do you seek from the Great Rat King? \
    \You explain that the miners would like to pass. \
    \I shall grant your demand if you can answer my riddles. But if you fail, you shall perish at my hand. Do you take up the challenge?(Type yes or no)")
    Data.Map.empty
    (Data.Map.fromList [(Types.Back, "Tunnel diggers")])

second_tunnel_1 :: Types.Location
second_tunnel_1 = Types.Location
    "Second tunnel 1"
    (const "You reach the end of the tunnel. The light you saw comes from a tiny hole in the left wall of the tunnel.")
    Data.Map.empty
    (Data.Map.fromList [(Types.Back, "Tunnel diggers")])

third_tunnel :: Types.Location
third_tunnel = Types.Location
    "Third tunnel"
    (const "Navigating the shadowy length of the third tunnel, you notice the air \
    \gradually fill with an unplaceable but comforting aroma, reminiscent \
    \of ancient texts and whispered secrets. The walls, subtly adorned with \
    \symbols that speak of community and stars aligning, lead you on with the \
    \promise of discovery. A faint, melodic hum, akin to distant singing, \
    \encourages each step forward, hinting at the existence of a sacred space \
    \ahead. As the passage unfolds into a serene chamber bathed in a soft, \
    \ethereal light, the ambiance suggests you're on the verge of entering a \
    \place of deep cultural significance, perhaps a sanctuary where history and \
    \spirituality converge in silent communion.")
    Data.Map.empty
    (Data.Map.fromList [(Types.Back, "In front of third tunnel"), (Types.Forward, "Synagogue")])

synagogue :: Types.Location
synagogue = Types.Location
    "Synagogue"
    (const "As you reach the end of the tunnel, the ambiance shifts dramatically. \
    \You find yourself in a spacious, time-honored synagogue. \
    \Sunlight streams through stained glass windows, casting a kaleidoscope \
    \of colors across the polished stone floor. The air is filled with a \
    \sense of peace and ancient wisdom. Rows of wooden pews lead your gaze \
    \to the ornate Ark at the far end, where the Torah scrolls are kept. \
    \You realize the journey through the tunnels was not just a test of survival, \
    \but a journey of discovery, leading you to this place of profound spiritual significance. \
    \Congratulations, you have found the hidden synagogue and completed your adventure!")
    Data.Map.empty
    (Data.Map.fromList [(Types.Back, "Third tunnel")])


isAligatorInLocation :: String -> Data.Map.Map String Location -> Bool
isAligatorInLocation locationName locationsMap = 
    case Data.Map.lookup locationName locationsMap of
        Just location -> Data.Map.member Items.aligator (items location)
        Nothing -> False

removeAligatorFromLocation :: String -> Data.Map.Map String Location -> Data.Map.Map String Location
removeAligatorFromLocation locationName locationsMap = 
    case Data.Map.lookup locationName locationsMap of
        Just location -> Data.Map.insert locationName (location { items = Data.Map.delete Items.aligator (items location) }) locationsMap
        Nothing -> locationsMap