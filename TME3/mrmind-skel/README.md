# TME 3 : MrMind

## Projet MrMind à compléter.

 - dans `src/MindEngine.hs` se trouve le coeur du programme, certaines fonctions ne sont pas définies 
(cherchez les `undefined`)

 - dans `test/MindEngineSpec.hs` se trouve les tests unitaire (HSpec) du coeur. Les tests pour les fonctions
à compléter sont écrits. La fonction `verify` n'est pas correctement testée (tests à compléter)


- toutes les fonctions du noyau doivent être testées
- `stack test` doit valider tous les tests

## Défi : Un vrai MrMind

Compléter le projet en ajoutant les modes de jeu proposés dans le point d'entrée du programme.
(pour avoir tous les points)

## Références

Voici quelques pointeurs utiles pour compléter/comprendre le projet :

 - guide du jeu de Mastermind : <https://fr.wikipedia.org/wiki/Mastermind>

 - les tests unitaires avec HSpec : <https://hspec.github.io/>

 - entrées/sorties en Haskell : <http://learnyouahaskell.com/input-and-output>
   (notamment la partie *Randomness* pour le défi)
