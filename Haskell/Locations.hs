module Location where 

import Item

data Direction = Forward | Back | Left | Right deriving (Eq, Ord)

data Path = Path{
    direction::Direction,
    to::String
} deriving(Eq)

data Location = Location{
    name::String,
    description::String,
    items::[Item],
    pahts::[Path]
} deriving(Eq)