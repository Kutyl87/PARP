module Locations where 

import Data.Map (Map, fromList, toList, lookup, empty)
import Items

data Direction = Forward | Back | Left | Right deriving (Eq, Ord)

strToDir::String->Maybe Direction
strToDir s | s == "forward" = Just Locations.Forward
        | s == "back" = Just Locations.Back
        | s == "left" = Just Locations.Left
        | s == "right" = Just Locations.Right
        | otherwise = Nothing

getLocationStringAtDir::Location->Direction->Maybe String
getLocationStringAtDir l d = Data.Map.lookup d (paths l)
    
data Location = Location{
    name::String,
    description::String,
    items::Data.Map.Map String Int,
    paths::Map Direction String
}

printItemList::[(String, Int)]->String
printItemList [] = ""
printItemList (x:xs) = fst x ++ " (count: " ++ show (snd x) ++ ")\n" ++ printItemList xs

listItems::Location->String
listItems l = printItemList (Data.Map.toList (Locations.items l))


entrance :: Location
entrance = Location
    "Entrance"
    "You are in the entrance. You notice doors behind you, and you feel pain inside your head. There is a small note at the floor. Maybe you should inspect it?"
    (Data.Map.fromList [(Items.note, 1)])
    (Data.Map.fromList [(Locations.Forward, "Hall")])

hall :: Location
hall = Location
    "Hall"
    "There is a hall with 3 tunnels. There must be a way to get out of there."
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "Entrance"), (Locations.Right, "In front of first tunnel"), (Locations.Forward, "In front of second tunnel"), (Locations.Left, "In front of third tunnel")])

in_front_of_first_tunnel :: Location
in_front_of_first_tunnel = Location
    "In front of first tunnel"
    "You are standing in front of the rightmost tunnel."
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "Hall"), (Locations.Forward, "First tunnel"), (Locations.Left, "In front of second tunnel")])

in_front_of_second_tunnel :: Location
in_front_of_second_tunnel = Location
    "In front of second tunnel"
    "You are standing in front of the middle tunnel."
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "Hall"), (Locations.Forward, "Second tunnel"), (Locations.Right, "In front of first tunnel"), (Locations.Left, "In front of third tunnel")])

in_front_of_third_tunnel :: Location
in_front_of_third_tunnel = Location
    "In front of third tunnel"
    "You are standing in front of the leftmost tunnel."
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "Hall"), (Locations.Forward, "Third tunnel"), (Locations.Right, "In front of second tunnel")])

first_tunnel :: Location
first_tunnel = Location
    "First tunnel"
    "You are in the first tunnel. You can smell blood inside. It seems to be a bad way to escape."
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "In front of first tunnel"), (Locations.Forward, "Dealer room")])

dealer_room :: Location
dealer_room = Location
    "Dealer room"
    "You have entered a Jewish dealer''s space. He wants to sell you a magic flute, but he do not specified its aim. Maybe it can be useful? He wants to help him, it will cost you 50 energy. (use buy(flute) to buy the flute)"
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "First tunnel"), (Locations.Forward, "Aligator room")])

aligator_room :: Location
aligator_room = Location
    "Aligator room"
    "You have entered an Aligator space. There is a huge reptile at the back. Fight could be difficult and demanding. Would you try? (Type fight(aligator) to fight)"
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "Dealer room"), (Locations.Forward, "End of first tunnel")])

end_of_first_tunnel :: Location
end_of_first_tunnel = Location
    "End of first tunnel"
    "The sound of water crashing forcefully ahead fills the air. Advancing might prove to be unwise."
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "Aligator room"), (Locations.Forward, "Waterfall")])

waterfall :: Location
waterfall = Location
    "Waterfall"
    "You have entered a waterfall. You can see a light at the end of the tunnel. You are carried away by the current of water."
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "End of first tunnel")])

second_tunnel :: Location
second_tunnel = Location
    "Second tunnel"
    "You are in the second tunnel. You can see a light at the end of a tunnel. Maybe this is a way to escape?"
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "In front of second tunnel"), (Locations.Forward, "Tunnel diggers")])

tunnel_diggers :: Location
tunnel_diggers = Location
    "Tunnel diggers"
    "You see a few people with mining tools. They tell you that they''re trying to dig a tunnel to the surface but a herd of rats stands in their way. They point at a crudely built tunnel entrance to the left. Prehaps they will help you if you slay the rats?"
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "Second tunnel"), (Locations.Forward, "Second tunnel 1"), (Locations.Left, "Side tunnel")])

side_tunnel :: Location
side_tunnel = Location
    "Side tunnel"
    "You are in a narrow, crudely built tunnel. Deep inside you can see the towering shadow of a giant rat. \
    \Greeting, traveler. What do you seek from the Great Rat King? \
    \You explain that the miners would like to pass. \
    \I shall grant your demand if you can answer my riddles. But if you fail, you shall perish at my hand. Do you take up the challenge?(Type yes or no)"
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "Tunnel diggers")])

second_tunnel_1 :: Location
second_tunnel_1 = Location
    "Second tunnel 1"
    "You reach the end of the tunnel. The light you saw comes from a tiny hole in the left wall of the tunnel."
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "Tunnel diggers")])

third_tunnel :: Location
third_tunnel = Location
    "Third tunnel"
    "Navigating the shadowy length of the third tunnel, you notice the air \
    \gradually fill with an unplaceable but comforting aroma, reminiscent \
    \of ancient texts and whispered secrets. The walls, subtly adorned with \
    \symbols that speak of community and stars aligning, lead you on with the \
    \promise of discovery. A faint, melodic hum, akin to distant singing, \
    \encourages each step forward, hinting at the existence of a sacred space \
    \ahead. As the passage unfolds into a serene chamber bathed in a soft, \
    \ethereal light, the ambiance suggests you're on the verge of entering a \
    \place of deep cultural significance, perhaps a sanctuary where history and \
    \spirituality converge in silent communion."
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "In front of third tunnel"), (Locations.Forward, "Synagogue")])

synagogue :: Location
synagogue = Location
    "Synagogue"
    "As you reach the end of the tunnel, the ambiance shifts dramatically. \
    \You find yourself in a spacious, time-honored synagogue. \
    \Sunlight streams through stained glass windows, casting a kaleidoscope \
    \of colors across the polished stone floor. The air is filled with a \
    \sense of peace and ancient wisdom. Rows of wooden pews lead your gaze \
    \to the ornate Ark at the far end, where the Torah scrolls are kept. \
    \You realize the journey through the tunnels was not just a test of survival, \
    \but a journey of discovery, leading you to this place of profound spiritual significance. \
    \Congratulations, you have found the hidden synagogue and completed your adventure!"
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "Third tunnel")])