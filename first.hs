doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

factorial :: (Eq p, Num p) => p -> p
factorial 1 = 1
factorial x = x * factorial (x - 1)

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r