module FoodChain (song) where

import Data.List (tails)

firstline x = "I know an old lady who swallowed a " ++ x ++ ".\n"

exclamation "fly" = ""
exclamation "spider" = "It wriggled and jiggled and tickled inside her.\n"
exclamation "bird" = "How absurd to swallow a bird!\n" 
exclamation "cat" = "Imagine that, to swallow a cat!\n"
exclamation "dog" = "What a hog, to swallow a dog!\n"
exclamation "goat" = "Just opened her throat and swallowed a goat!\n"
exclamation "cow" = "I don't know how she swallowed a cow!\n"
exclamation "horse" = "She's dead, of course!\n"

toCatch a w = "She swallowed the " ++ a ++ " to catch the " ++ w ++ ".\n" 

reason "cow" = "cow" `toCatch` "goat"
reason "goat" = "goat" `toCatch` "dog"
reason "dog" = "dog" `toCatch` "cat"
reason "cat" = "cat" `toCatch` "bird"
reason "bird" = "bird" `toCatch` "spider that wriggled and jiggled and tickled inside her"
reason "spider" = "spider" `toCatch` "fly"
reason "fly" = "I don't know why she swallowed the fly. Perhaps she'll die.\n"

strofa [] = ""
strofa ("horse":_)      = firstline "horse" ++ exclamation "horse" 
strofa animals@(x:_)    = firstline x ++ exclamation x ++ concatMap reason animals ++ "\n"

song :: String
song = concatMap strofa $ reverse $ tails ["horse", "cow", "goat", "dog", "cat", "bird", "spider", "fly"]
