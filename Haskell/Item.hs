module Item where

data Item = Item{
    name::String,
    description::String,
    recipe::[Item]
} deriving(Eq)