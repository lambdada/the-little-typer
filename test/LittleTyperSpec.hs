module LittleTyperSpec where

import           LittleTyper
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = describe "The Little Typer" $
  describe "The law of tick marks: Atoms" $
  it "an atom is a tick mark followed by one or more letters or hyphens" $ do
    isAnAtom "'atom" `shouldBe` True
    isAnAtom "'ratatouille" `shouldBe` True
    isAnAtom "---" `shouldBe` False
    isAnAtom "'" `shouldBe` False
    isAnAtom "'at0m" `shouldBe` False
    isAnAtom "'coeur-d-artichauts" `shouldBe` True
    isAnAtom "'coeur_d_artichauts" `shouldBe` False
    isAnAtom "'αβγδ" `shouldBe` True
