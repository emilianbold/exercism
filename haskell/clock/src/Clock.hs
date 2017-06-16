module Clock (clockHour, clockMin, fromHourMin, toString) where

data Clock = Clock Int Int
    deriving (Eq, Show)

instance Num Clock where
    fromInteger n = fromHourMin (x `div` 60) (x `mod` 60)
        where x = fromIntegral n

    (+) (Clock h m) (Clock h2 m2) = fromHourMin (h + h2) (m + m2)

    negate (Clock h m) = Clock (23 - h) (60 - m)

clockHour :: Clock -> Int
clockHour (Clock h _) = h 

clockMin :: Clock -> Int
clockMin (Clock _ m) = m 

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock ((hour + min `div` 60) `mod` 24) (min `mod` 60)

toString :: Clock -> String
toString (Clock h m) = twoDigits h  ++ ":" ++ twoDigits m
        where twoDigits n
                | n > 9     = show n
                | otherwise = '0' : show n 
