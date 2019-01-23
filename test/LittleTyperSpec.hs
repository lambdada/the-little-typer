module LittleTyperSpec where

import           LittleTyper     hiding (isAnAtom)
import qualified LittleTyper     as T
import           Test.Hspec
import           Test.QuickCheck

isAnAtom :: String -> SpecWith ()
isAnAtom s =
  it ("an atom is a tick mark followed by one or more letters or hyphens: " <> s) $  T.isAnAtom s `shouldBe` True

isNotAnAtom :: String -> SpecWith ()
isNotAnAtom s =
  it ("an atom is a tick mark followed by one or more letters or hyphens: " <> s) $ T.isAnAtom s `shouldBe` False

spec :: Spec
spec =
  describe "The Little Typer" $ do
  describe "Atoms" $ do
    isAnAtom "'atom"
    isAnAtom "'ratatouille"
    isAnAtom "'coeur-d-artichauts"
    isAnAtom "'αβγδ"
  describe "Not Atoms" $ do
    isNotAnAtom "---"
    isNotAnAtom "'"
    isNotAnAtom "'at0m"
    isNotAnAtom "'coeur_d_artichauts"

  it "can be judged as an Atom" $ do
    parse "'atom"`shouldBe` (Right $ Atom "atom")

  it "compares 2 atoms" $ do
    parse "'ratatouille" `shouldBe` parse "'ratatouille"

  -- it "two atoms with the same characters are the 'same' Atom" $ do
  --   return value
