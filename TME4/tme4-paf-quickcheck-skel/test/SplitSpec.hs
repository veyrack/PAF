module SplitSpec where

import Test.Hspec
import Test.QuickCheck

import Split

splitSpec0 = do
  describe "split" $ do
    it "splits a string wrt. a given character into a list of words" $
      (split '/' "aa/bb/ccc/dd d") `shouldBe` ["aa", "bb", "ccc", "dd d"] -- on verifie que la fonction split bien la chaine en fonction du separateur

splitSpec1 = do
  describe "split" $ do
    it "can be undone with unsplit (v1)" $ property $ -- le `property` permet d'ajouter un test de propriété QuickCheck
                                                      -- au milieu d'une spécification de test HSpec.
      \c xs -> collect (length xs) $ -- on affiche les tailles de chaîne
               prop_split_unsplit c xs -- on verifie la propriete


splitSpec2 = do
  describe "split" $ do
    it "can be undone with unsplit (v2)" $ property $
      \xs -> forAll (elements xs) $ \c -> collect (length xs) $ prop_split_unsplit c xs -- on verifie que pour chaque element de xs 
                                                                                        -- la propriete de split unsplit est verifiee

-- Remarque : on utilise comme caractère de split les éléments des listes `xs` en entrée,
--            cf. la doc QuickCheck sur `forAll`, `elements`, etc.


splitSpec3 = do
  describe "split" $ do
    it "can be undone with unsplit (v3)" $ property $
      forAll (oneof [return "bla bla bli"
                     , return "toto"
                     , return ""
                     , return "un    deux trois   quatre"]) $ -- pour chaque element de la liste
      \xs -> prop_split_unsplit ' ' xs -- on verifie la propriete

