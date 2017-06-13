module Beer (song) where

song :: String
song = concatMap (\n -> line n ++ newline ++ secondLine n) [99,98..0]

newline :: String
newline = "\n"

line :: Int -> String
line 1 = "1 bottle of beer on the wall, 1 bottle of beer."
line 0 = "No more bottles of beer on the wall, no more bottles of beer."
line n = (show n) ++ " bottles of beer on the wall, " ++ (show n) ++ " bottles of beer."

secondLine :: Int -> String
secondLine 2 = "Take one down and pass it around, 1 bottle of beer on the wall.\n\n"
secondLine 1 = "Take it down and pass it around, no more bottles of beer on the wall.\n\n"
secondLine 0 = "Go to the store and buy some more, 99 bottles of beer on the wall.\n"
secondLine n = "Take one down and pass it around, " ++ (show (n - 1)) ++ " bottles of beer on the wall.\n\n";
