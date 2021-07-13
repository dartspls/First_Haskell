-- http://learnyouahaskell.com/syntax-in-functions

-- Pattern matching with catch-all
lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven!"
lucky x = "sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

-- Recursive factorial. Could replace "pred n" with "n - 1". Just trying to be fancy
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (pred n)