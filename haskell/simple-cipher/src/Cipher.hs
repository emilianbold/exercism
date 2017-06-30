module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Control.Monad(foldM)
import Data.Char(chr, ord)
import Data.List(zip)
import System.Random

caesarDecode :: String -> String -> String
caesarDecode key encodedText = 
    encode (map (\c -> ord 'a' - ord c) key) encodedText 

caesarEncode :: String -> String -> String
caesarEncode key text = encode (map (\c -> ord c - ord 'a') key) text

encode:: [Int] -> String -> String
encode distance text = 
    map transform $ zip text $ cycle distance 
    where az = ord 'z' - ord 'a' + 1
          transform (c, dist)= chr (ord 'a' + (ord c - ord 'a' + dist + az) `mod` az)

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
    gen <- newStdGen
    let key = generateKey gen
    return (key, caesarEncode key text)

generateKey :: StdGen -> String
generateKey gen = fst $ foldl process ([], gen) [0..az]
                  where az = ord 'z' - ord 'a' + 1
                        process:: (String, StdGen) -> Int -> (String, StdGen)
                        process (key, g) c = let (r, g2) = randomR (ord 'a', ord 'z') g
                                                 in(chr r : key, g2)
