isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
