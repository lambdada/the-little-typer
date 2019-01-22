module LittleTyper
    ( isAnAtom
    ) where

import           Data.Char

isAnAtom :: String -> Bool
isAnAtom ('\'':x:xs) = all isValidAtomCharacter (x:xs)
  where
    isValidAtomCharacter c
        | isAlpha c = True
        | c == '-'  = True
        | otherwise = False
isAnAtom _           = False
