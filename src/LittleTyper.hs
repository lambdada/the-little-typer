module LittleTyper
    ( isAnAtom
    , parse
    , Thing(..)
    ) where

import           Data.Char
import           Text.Parsec hiding (parse)

data Thing = Atom { atom :: String }
           | SomethingElse
  deriving (Eq, Show)

parse :: String -> Either String Thing
parse  input  =
    case runParser littleTyperParser () "" input of
        Right x -> Right x
        Left e  -> Left $ show e

isAnAtom :: String -> Bool
isAnAtom s = case parse s of
    Right (Atom _) -> True
    Left _         -> False

littleTyperParser :: Parsec String () Thing
littleTyperParser = atomParser <* eof

atomParser :: Parsec String () Thing
atomParser = Atom <$> ( char '\'' *> many1 validAtomCharacter)
    where
    validAtomCharacter = letter <|> char '-'
