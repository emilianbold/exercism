module SecretHandshake (handshake) where

import Data.Bits (testBit)

handshake :: Int -> [String]
handshake n =
    reverseIf (testBit n 4) $ map word $ filter (testBit n) [0..3]
    where word 0 = "wink"
          word 1 = "double blink"
          word 2 = "close your eyes"
          word 3 = "jump"
          reverseIf True  x = reverse x
          reverseIf False x = x
