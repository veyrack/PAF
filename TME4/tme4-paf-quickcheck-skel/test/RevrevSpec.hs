module RevrevSpec where

import Test.Hspec
import Test.QuickCheck

import Revrev

revrevSpec = do
  describe "reverse" $ do
    context "when used with ints" $ do
      it "is idempotent" $ property $    -- le `property` permet d'ajouter un test de propriété QuickCheck
                                         -- au milieu d'une spécification de test HSpec.
        \xs ys -> collect (length xs) $    -- on affiche les tailles de chaîne
                                           -- pour avoir une idée de la distribution aléatoire
                  prop_revrev (xs :: [Int]) ys  -- on vérifie la propriété pour des listes d'entiers arbitraires

